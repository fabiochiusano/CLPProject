package toolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    // Store the current token, as read from the lexer.
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD && tokens.hasNext) {
          currentToken = tokens.next
        }
      }
    }

    // ''Eats'' the expected token, or terminates with an error.
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    // Complains that what was found was not expected.
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      println(currentToken.position);
      fatal("expected: " + (kind :: more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    /*
     * Divide the work of the parser in many subfunctions.
     * Two main function: getMainObject an getAllClasses to parse the entire program,
     * then many more other function to parse specific parts of the main object and all
     * other classes, such as variable declarations, method calls, statements and expressions.
    */
    def parseGoal: Program = {

      //Helper function that tries to wrap current token into identifier class, and throw error if cannot do it
      def getID(): Identifier = {
        val id = currentToken match {
          case ID(s) => Identifier(s).setPos(currentToken)
          case _     => expected(IDKIND)
        }
        eat(IDKIND); id
      }

      //Helper function that tries to match current token to legal types, and throw error if cannot do it
      def getType(): TypeTree = {
        val pos = currentToken
        currentToken.kind match {
          case STRING =>
            eat(STRING); StringType().setPos(pos)
          case BOOLEAN =>
            eat(BOOLEAN); BooleanType().setPos(pos)
          case INT =>
            eat(INT); currentToken.kind match {
              case LBRACKET =>
                eat(LBRACKET); eat(RBRACKET); IntArrayType().setPos(pos)
              case _ => IntType().setPos(pos)
            }
          case IDKIND => getID() //User defined type, such as classes
          case _      => expected(STRING, BOOLEAN, INT, IDKIND)
        }
      }

      //Function to get the main object of the program, always starting with:
      //object id { def main (): Unit = { statements* } }
      def getMainObject(): MainObject = {
        eat(OBJECT)
        val id = getID()
        eat(LBRACE)
        eat(DEF); eat(MAIN); eat(LPAREN); eat(RPAREN); eat(COLON); eat(UNIT); eat(EQSIGN); eat(LBRACE)
        val stats = getAllStatements()
        eat(RBRACE); eat(RBRACE)
        MainObject(id, stats).setPos(id)
      }

      //Function to iterate over all classes of the program
      def getAllClasses(): List[ClassDecl] = {
        def iter(list: List[ClassDecl]): List[ClassDecl] = currentToken.kind match {
          case EOF   => list
          case CLASS => iter(list :+ getClass())
          case _     => expected(CLASS)
        }
        iter(List())
      }

      //Function to get a single class declaration, with an optional superclass indication
      def getClass(): ClassDecl = {
        eat(CLASS)
        val id = getID()
        val p = if (currentToken.kind == EXTENDS) {
          eat(EXTENDS)
          val parentID = getID()
          Some(parentID)
        } else None
        eat(LBRACE)
        val vars = getAllVar()
        val nativeMeths = getAllNativeMeth()   //Custom extension: parse native methods
        val optionNativeMeths = if (nativeMeths == Nil) None else Some(nativeMeths)  //Custom extension
        val meths = getAllMeth()
        eat(RBRACE)
        ClassDecl(id, p, vars, meths, optionNativeMeths).setPos(id)
      }

      //Function to iterate over all variable declarations (always at the top of a class)
      def getAllVar(): List[VarDecl] = {
        def iter(list: List[VarDecl]): List[VarDecl] = currentToken.kind match {
          case VAR => iter(list :+ getVar())
          case _   => list
        }
        iter(List())
      }

      //Function to get a single variable declaration in the form: "var id : type"
      def getVar(): VarDecl = {
        eat(VAR)
        val id = getID()
        eat(COLON)
        val t = getType()
        eat(SEMICOLON)
        VarDecl(t, id).setPos(id)
      }

      //Function to iterate over all method declarations inside a class
      def getAllMeth(): List[MethodDecl] = {
        def iter(list: List[MethodDecl]): List[MethodDecl] = currentToken.kind match {
          case DEF => iter(list :+ getMeth())
          case _   => list
        }
        iter(List())
      }
      
      //Custom extension: parse native methods
      def getAllNativeMeth(): List[NativeMethodDecl] = {
        def iter(list: List[NativeMethodDecl]): List[NativeMethodDecl] = currentToken.kind match {
          case NATIVE => iter(list :+ getNativeMeth())
          case _   => list
        }
        iter(List())
      }
      
      //Custom extension: parse a native method
      def getNativeMeth(): NativeMethodDecl = {
        eat(NATIVE)
        eat(DEF)
        val id = getID()
        eat(LPAREN)
        val args = getArgs()
        eat(RPAREN); eat(COLON)
        val retType = getType()
        NativeMethodDecl(retType, id, args).setPos(id)
      }

      //Function to get a single method declaration of the form: "def id ( args* ): type = { stats*; return expr }"
      def getMeth(): MethodDecl = {
        eat(DEF)
        val id = getID()
        eat(LPAREN)
        val args = getArgs()
        eat(RPAREN); eat(COLON)
        val retType = getType()
        eat(EQSIGN); eat(LBRACE)
        val vars = getAllVar()
        val stats = getAllStatements()
        eat(RETURN)
        val retExpr = getExpr()
        eat(SEMICOLON); eat(RBRACE)
        MethodDecl(retType, id, args, vars, stats, retExpr).setPos(id)
      }

      //Function to iterate over all formal arguments of a method declaration
      def getArgs(): List[Formal] = {
        def iter(list: List[Formal]): List[Formal] = currentToken.kind match {
          case RPAREN => list
          case COMMA =>
            if (list.isEmpty) expected(IDKIND) else eat(COMMA); iter(list :+ getArg())
          case _ => if (list.isEmpty) iter(list :+ getArg()) else expected(COMMA, RPAREN)
        }
        iter(List())
      }

      //Function to get a single formal argument of the type: "id : type"
      def getArg(): Formal = {
        val id = getID()
        eat(COLON)
        val t = getType()
        Formal(t, id).setPos(id)
      }

      //Function to iterate over all statements until a '}' or a RETURN are found 
      def getAllStatements(): List[StatTree] = {
        def iter(list: List[StatTree]): List[StatTree] = currentToken.kind match {
          case RBRACE => list
          case RETURN => list
          case _      => iter(list :+ getStat())
        }
        iter(List())
      }

      //Function to get a single statement, ending with a SEMICOLON
      def getStat(): StatTree = {
        val pos = currentToken
        currentToken.kind match {
          //Block
          case LBRACE =>
            eat(LBRACE); val stats = getAllStatements(); eat(RBRACE); Block(stats).setPos(pos)
          //If then else
          case IF => {
            eat(IF); eat(LPAREN)
            val e = getExpr()
            eat(RPAREN)
            val thn = getStat()
            val els = if (currentToken.kind == ELSE) { eat(ELSE); Some(getStat()) } else None
            If(e, thn, els).setPos(pos)
          }
          //While loop
          case WHILE => {
            eat(WHILE); eat(LPAREN)
            val e = getExpr()
            eat(RPAREN)
            val stat = getStat()
            While(e, stat).setPos(pos)
          }
          //Println
          case PRINTLN =>
            eat(PRINTLN); eat(LPAREN); val e = getExpr(); eat(RPAREN); eat(SEMICOLON); Println(e).setPos(pos)
          //Assignment (simple or of an array)
          case IDKIND => {
            val id = getID()
            currentToken.kind match {
              case EQSIGN =>
                eat(EQSIGN); val e2 = getExpr(); eat(SEMICOLON); Assign(id, e2).setPos(pos)
              case LBRACKET =>
                eat(LBRACKET); val e = getExpr(); eat(RBRACKET); eat(EQSIGN);
                val e2 = getExpr(); eat(SEMICOLON); ArrayAssign(id, e, e2).setPos(pos)
              case _ => expected(EQSIGN, LBRACKET)
            }
          }
          //Error, token not matching any of the previous statements
          case _ => expected(LBRACE, IF, WHILE, PRINTLN, IDKIND)
        }
      }

      //Function to get an expression, uses a map to define precedence of binary operators (in order to
      // easily extend with new operators)
      //Divide the work over some subclasses in order to make everything simpler, scalable and more manteinable
      def getExpr(): ExprTree = {
        val precedenceMap: Map[TokenKind, Int] = Map(OR -> 1, AND -> 2, LESSTHAN -> 3, EQUALS -> 3,
          PLUS -> 4, MINUS -> 4, TIMES -> 5, DIV -> 5)

        //Function to get a simple expression (the ones not involving binary operators)
        def getSimpleExpr(): ExprTree = {
          val pos = currentToken
          currentToken.kind match {
            //true keyword
            case TRUE =>
              eat(TRUE); True().setPos(pos)
            //false keyword
            case FALSE =>
              eat(FALSE); False().setPos(pos)
            //this keyword (takes care of possible concatenated method call)
            case THIS =>
              eat(THIS); currentToken.kind match {
                case DOT =>
                  eat(DOT); getMethodCall(This())
                case _ => This().setPos(pos)
              }
            //new keyword, used for both classes and array types (takes care of possible concatenated method call)
            case NEW => {
              eat(NEW)
              currentToken.kind match {
                case INT =>
                  eat(INT); eat(LBRACKET); val e = getExpr(); eat(RBRACKET); NewIntArray(e).setPos(pos)
                case _ => {
                  val id = getID()
                  eat(LPAREN); eat(RPAREN);
                  currentToken.kind match {
                    case DOT =>
                      eat(DOT); getMethodCall(New(id))
                    case _ => New(id).setPos(pos)
                  }
                }
              }
            }
            //! (not) unary operator
            case BANG =>
              eat(BANG); Not(getSimpleExpr()).setPos(pos)
            //Parenthesized expression (takes care of possible concatenated method call, array read or ".length")
            case LPAREN =>
              eat(LPAREN); val e = getExpr(); eat(RPAREN); currentToken.kind match {
                case DOT =>
                  eat(DOT); currentToken.kind match {
                    case LENGTH =>
                      eat(LENGTH); ArrayLength(e).setPos(e)
                    case IDKIND => getMethodCall(e)
                    case _      => expected(LENGTH, IDKIND)
                  }
                case LBRACKET =>
                  eat(LBRACKET); val e2 = getExpr(); eat(RBRACKET); ArrayRead(e, e2).setPos(e)
                case _ => e.setPos(pos)
              }
            //Integer literals
            case INTLITKIND => {
              val n = currentToken match {
                case INTLIT(n) => n
                case _         => expected(INTLITKIND)
              }
              eat(INTLITKIND); IntLit(n).setPos(pos)
            }
            //String literals
            case STRLITKIND => {
              val s = currentToken match {
                case STRLIT(s) => s
                case _         => expected(STRLITKIND)
              }
              eat(STRLITKIND); StringLit(s).setPos(pos)
            }
            //Method call or array read or ".length"
            case IDKIND => {
              val id = getID()
              currentToken.kind match {
                case DOT =>
                  eat(DOT); currentToken.kind match {
                    case LENGTH =>
                      eat(LENGTH); ArrayLength(id).setPos(id)
                    case IDKIND => getMethodCall(id)
                    case _      => expected(LENGTH, IDKIND)
                  }
                case LBRACKET =>
                  eat(LBRACKET); val e = getExpr(); eat(RBRACKET); ArrayRead(id, e).setPos(id)
                case _ => id
              }
            }
            //Error, token not matching any of the legal starting token of the expected simple expression
            case _ => expected(TRUE, FALSE, IDKIND, THIS, NEW, BANG, LPAREN, INTLITKIND, STRLITKIND)
          }
        }

        //Function to take care of linked method calls, like:
        // "new MakeLab().Initialize(75,75,50,4,12345).CreateRooms(5,5,100).Print()"
        //Takes care also of the case of a method call followed by array read or ".length"
        def getMethodCall(obj: ExprTree): ExprTree = {
          val id = getID()
          eat(LPAREN); val exprs = getAllParams(); eat(RPAREN);
          val e = MethodCall(obj, id, exprs).setPos(id)
          currentToken.kind match {
            case DOT =>
              eat(DOT); currentToken.kind match {
                case LENGTH =>
                  eat(LENGTH); ArrayLength(e).setPos(id)
                case IDKIND => getMethodCall(e)
                case _      => expected(LENGTH, IDKIND)
              }
            case LBRACKET =>
              eat(LBRACKET); val e2 = getExpr(); eat(RBRACKET); ArrayRead(e, e2).setPos(id)
            case _ => e
          }
        }

        //Function that concatenates an expression with the following ones, based on
        // binary operators precedence values
        def concatenateExpr(prec: Int, e: ExprTree): ExprTree = {
          currentToken.kind match {
            case op if ((precedenceMap contains op) && precedenceMap(op) > prec) => {
              eat(op)
              val simpExp = getSimpleExpr()
              val expr = putTogheter(e, op, concatenateExpr(precedenceMap(op), simpExp))
              concatenateExpr(prec, expr)
            }
            case op if ((precedenceMap contains op) && precedenceMap(op) <= prec) => e
            case _ => e
          }
        }

        //Function that puts together in the appropriate ExprTree class two expressions,
        // based on the given binary operator
        def putTogheter(e1: ExprTree, op: TokenKind, e2: ExprTree): ExprTree = op match {
          case PLUS     => Plus(e1, e2).setPos(e1)
          case MINUS    => Minus(e1, e2).setPos(e1)
          case TIMES    => Times(e1, e2).setPos(e1)
          case DIV      => Div(e1, e2).setPos(e1)
          case LESSTHAN => LessThan(e1, e2).setPos(e1)
          case EQUALS   => Equals(e1, e2).setPos(e1)
          case AND      => And(e1, e2).setPos(e1)
          case OR       => Or(e1, e2).setPos(e1)
          case _        => expected(PLUS, MINUS, TIMES, DIV, LESSTHAN, EQUALS, AND, OR)
        }

        //Start of the recursion in the main function getExpr (0 is lower than any precedence values)
        concatenateExpr(0, getSimpleExpr())
      }

      //Function to iterate over all concrete parameters of a method call
      def getAllParams(): List[ExprTree] = {
        def iter(list: List[ExprTree]): List[ExprTree] = currentToken.kind match {
          case RPAREN => list
          case COMMA =>
            if (list.isEmpty) expected(IDKIND) else eat(COMMA); iter(list :+ getExpr)
          case _ => if (list.isEmpty) iter(list :+ getExpr) else expected(RPAREN, COMMA)
        }
        iter(List())
      }

      //Start of the recursion in the main function parseGoal
      new Program(getMainObject, getAllClasses)
    }

    // Initialize the first token
    readToken

    // Parse
    parseGoal
  }
}
