package toolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  def run(ctx: Context)(f: File): Iterator[Token] = {
    val lib = new File("./gui_lib/GUI.tool")
    var source = Source.fromChars((Source.fromFile(f) ++ "\n" ++ Source.fromFile(lib)).toArray)
    var currentChar: Char = source.next
    var shouldNextChar = false
    import ctx.reporter._

    def currentPos(): Positioned = new Positioned {}.setPos(f, source.pos)

    def readNextToken(): Token = {
      if (!source.hasNext)
        new Token(EOF).setPos(f, source.pos)
      else {
        if (shouldNextChar) currentChar = source.next
        while ((currentChar == ' ' || currentChar == '\n' || currentChar == '\t' || currentChar == '\r') && source.hasNext)
          currentChar = source.next // Delete white spaces and EOL
        val firstSourcePos = source.pos;
        shouldNextChar = true

        currentChar match {
          case ' '  => new Token(EOF).setPos(f, source.pos) // + 1)
          case '\n' => new Token(EOF).setPos(f, source.pos) // + 1)
          case '\r' => new Token(EOF).setPos(f, source.pos) // + 1)
          case '\t' => new Token(EOF).setPos(f, source.pos) // + 1)
          case ':'  => new Token(COLON).setPos(currentPos())
          case ';'  => new Token(SEMICOLON).setPos(currentPos())
          case '.'  => new Token(DOT).setPos(currentPos())
          case ','  => new Token(COMMA).setPos(currentPos())
          case '=' => {
            val secondChar = source.next
            if (secondChar == '=') {
              new Token(EQUALS).setPos(f, firstSourcePos)
            } else {
              shouldNextChar = false
              currentChar = secondChar
              new Token(EQSIGN).setPos(f, firstSourcePos)
            }
          }
          case '!' => new Token(BANG).setPos(currentPos())
          case '(' => new Token(LPAREN).setPos(currentPos())
          case ')' => new Token(RPAREN).setPos(currentPos())
          case '[' => new Token(LBRACKET).setPos(currentPos())
          case ']' => new Token(RBRACKET).setPos(currentPos())
          case '{' => new Token(LBRACE).setPos(currentPos())
          case '}' => new Token(RBRACE).setPos(currentPos())
          case '&' => {
            val secondChar = source.next
            if (secondChar == '&') {
              new Token(AND).setPos(f, firstSourcePos)
            } else {
              shouldNextChar = false
              currentChar = secondChar
              error("Single &: ", new Positioned {}.setPos(f, firstSourcePos))
              new Token(BAD).setPos(f, firstSourcePos)
            }
          }
          case '|' => {
            val secondChar = source.next
            if (secondChar == '|') {
              new Token(OR).setPos(f, firstSourcePos)
            } else {
              shouldNextChar = false
              currentChar = secondChar
              error("Single |: ", new Positioned {}.setPos(f, firstSourcePos))
              new Token(BAD).setPos(f, firstSourcePos)
            }
          }
          case '<' => new Token(LESSTHAN).setPos(currentPos())
          case '+' => new Token(PLUS).setPos(currentPos())
          case '-' => new Token(MINUS).setPos(currentPos())
          case '*' => new Token(TIMES).setPos(currentPos())
          case '/' => {
            var otherChar = source.next
            if (otherChar == '/') {
              while (otherChar != '\n' && otherChar != '\r' && source.hasNext) {
                otherChar = source.next
              }
              readNextToken()
            } else if (otherChar == '*') {
              otherChar = source.next
              var endOfCommentFound = false
              while (!endOfCommentFound && source.hasNext) {
                if (otherChar == '*') {
                  otherChar = source.next
                  if (otherChar == '/') {
                    endOfCommentFound = true
                  }
                } else {
                  otherChar = source.next
                }
              }
              if (!endOfCommentFound) error("Unterminated block comment: ", currentPos())
              readNextToken()
            } else {
              shouldNextChar = false
              currentChar = otherChar
              new Token(DIV).setPos(f, firstSourcePos)
            }
          }
          case c: Char => {
            if (c.isLetter) { // ID
              var str = "" + c
              var otherChar = c
              while ((otherChar.isLetterOrDigit || otherChar == '_') && source.hasNext) {
                otherChar = source.next
                if (otherChar.isLetterOrDigit || otherChar == '_') str += otherChar
              }
              currentChar = otherChar
              shouldNextChar = false
              str match {
                case "object"  => new Token(OBJECT).setPos(f, firstSourcePos)
                case "class"   => new Token(CLASS).setPos(f, firstSourcePos)
                case "native"  => new Token(NATIVE).setPos(f, firstSourcePos)  //Custom extension: new token NATIVE for native methods
                case "def"     => new Token(DEF).setPos(f, firstSourcePos)
                case "var"     => new Token(VAR).setPos(f, firstSourcePos)
                case "Unit"    => new Token(UNIT).setPos(f, firstSourcePos)
                case "main"    => new Token(MAIN).setPos(f, firstSourcePos)
                case "String"  => new Token(STRING).setPos(f, firstSourcePos)
                case "extends" => new Token(EXTENDS).setPos(f, firstSourcePos)
                case "Int"     => new Token(INT).setPos(f, firstSourcePos)
                case "Bool"    => new Token(BOOLEAN).setPos(f, firstSourcePos)
                case "while"   => new Token(WHILE).setPos(f, firstSourcePos)
                case "if"      => new Token(IF).setPos(f, firstSourcePos)
                case "else"    => new Token(ELSE).setPos(f, firstSourcePos)
                case "return"  => new Token(RETURN).setPos(f, firstSourcePos)
                case "length"  => new Token(LENGTH).setPos(f, firstSourcePos)
                case "true"    => new Token(TRUE).setPos(f, firstSourcePos)
                case "false"   => new Token(FALSE).setPos(f, firstSourcePos)
                case "this"    => new Token(THIS).setPos(f, firstSourcePos)
                case "new"     => new Token(NEW).setPos(f, firstSourcePos)
                case "println" => new Token(PRINTLN).setPos(f, firstSourcePos)
                case otherwise => new ID(otherwise).setPos(f, firstSourcePos)
              }
            } else if (c.isDigit) { // IntLiterals
              if (c == '0') new INTLIT(0).setPos(f, firstSourcePos)
              else {
                var str = "" + c
                var otherChar = c
                while (source.hasNext && otherChar.isDigit) {
                  otherChar = source.next
                  if (otherChar.isDigit) str += otherChar
                }
                currentChar = otherChar
                shouldNextChar = false
                new INTLIT(str.toInt).setPos(f, firstSourcePos)
              }
            } else if (c == '"') { // StrLiterals
              var str = ""
              var otherChar = source.next
              while ((otherChar != '"') && (otherChar != '\n' && otherChar != '\r') && source.hasNext) {
                str += otherChar
                otherChar = source.next
              }
              if (otherChar != '"') {
                try {
                  ctx.reporter.error("Unterminated string: ", new Positioned {}.setPos(f, firstSourcePos))
                } catch {
                  case _: Throwable => println("jhb")
                }
              }
              new STRLIT(str).setPos(f, firstSourcePos)
            } else {
              ctx.reporter.error("Invalid character: ", new Positioned {}.setPos(f, firstSourcePos))
              new Token(BAD).setPos(f, firstSourcePos)
            }
          }
        }
      }
    }

    new Iterator[Token] {
      var nextToken: Token = readNextToken
      var reachedEnd = false

      def hasNext = {
        nextToken.kind != EOF || !reachedEnd
      }

      def next = {
        val r = nextToken
        nextToken = readNextToken
        if (r.kind == EOF) {
          reachedEnd = true
        }
        r
      }
    }
  }
}
