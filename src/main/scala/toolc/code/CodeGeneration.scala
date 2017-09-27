package toolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{ New => _, _ }
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      //Generate class
      val parentOption = ct.parent match {
        case Some(id) => Some(id.value)
        case None     => None
      }
      val classFile =
        if (ct.id.value == "GUI") new ClassFile(ct.id.value, Some("ToolGUI")) //Custom extension: make the GUI class a subclass of ToolGUI
        else new ClassFile(ct.id.value, parentOption)
      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor

      //Add fields
      ct.vars.foreach { v => classFile.addField(getTypeAsString(v.getSymbol.getType), v.id.value) }

      //Add methods
      ct.methods.foreach { m =>
        val methodHandler = classFile.addMethod(getTypeAsString(m.getSymbol.getType), m.id.value,
          m.args.map { a => getTypeAsString(a.getSymbol.getType) })
        generateMethodCode(methodHandler.codeHandler, m)
      }

      //Custom extension: generate bytecode for native methods
      ct.nativeMethods match {
        case Some(list) =>
          list.foreach { m =>
            val methodHandler = classFile.addMethod(getTypeAsString(m.getSymbol.getType), m.id.value,
              m.args.map { a => getTypeAsString(a.getSymbol.getType) })
            generateNativeMethodCode(methodHandler.codeHandler, m)
          }
        case None =>
      }

      //Save class to file
      classFile.writeToFile((if (dir == "") "./" else dir) + ct.id.value + ".class")
    }

    def getTypeAsString(typ: Type): String = typ match {
      case TIntArray   => "[I"
      case TInt        => "I"
      case TBoolean    => "Z"
      case TString     => "Ljava/lang/String;"
      case TObject(id) => "L" + id.name + ";"
      case _           => sys.error("Internal error with: " + typ)
    }

    def returnType(typ: Type): ByteCode = typ match {
      case TIntArray   => ARETURN
      case TInt        => IRETURN
      case TBoolean    => IRETURN
      case TString     => ARETURN
      case TObject(id) => ARETURN
      case _           => sys.error("Internal error")
    }

    def XStore(n: Int, typ: Type): AbstractByteCodeGenerator = typ match {
      case TInt     => IStore(n)
      case TBoolean => IStore(n)
      case _        => AStore(n)
    }

    def XLoad(n: Int, typ: Type): AbstractByteCodeGenerator = typ match {
      case TInt     => ILoad(n)
      case TBoolean => ILoad(n)
      case _        => ALoad(n)
    }

    //Custom extension: generate bytecode for a native method 
    def generateNativeMethodCode(ch: CodeHandler, mt: NativeMethodDecl): Unit = {
      ch << ArgLoad(0)
      for(i <- 1 to mt.args.length) ch << ArgLoad(i)
      val argTypes = mt.args.map { a => getTypeAsString(a.getSymbol.getType) }.foldRight("")((a, b) => a + b)
      val methSignature = "(" + argTypes + ")" + getTypeAsString(mt.id.getType)
      ch << InvokeSpecial("ToolGUI", mt.id.value, methSignature)
      ch << returnType(mt.getSymbol.getType)
      ch.freeze
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      val varsMap = mt.vars.map { v => (v.id.value, ch.getFreshVar) }.toMap
      mt.stats.foreach { s => statToBytecode(ch, s, Some(methSym), varsMap) }

      ch << LineNumber(mt.retExpr.line)
      exprToBytecode(ch, mt.retExpr, Some(methSym), varsMap)
      ch << returnType(mt.getSymbol.getType)

      ch.freeze
    }

    def statToBytecode(ch: CodeHandler, stat: StatTree, symbol: Option[MethodSymbol], varsMap: Map[String, Int]): Unit = {
      ch << LineNumber(stat.line)

      stat match {
        case Block(stats) =>
          stats.foreach { s => statToBytecode(ch, s, symbol, varsMap) }

        case If(expr, thn, els) =>
          val elseLabel = ch.getFreshLabel("else")
          val afterLabel = ch.getFreshLabel("after")
          exprToBytecode(ch, expr, symbol, varsMap)
          ch << IfEq(elseLabel)
          statToBytecode(ch, thn, symbol, varsMap)
          els match {
            case Some(e) =>
              ch << Goto(afterLabel)
              ch << Label(elseLabel)
              statToBytecode(ch, e, symbol, varsMap)
              ch << Label(afterLabel)
            case None =>
              ch << Label(elseLabel)
          }

        case While(expr, stat) =>
          val condLabel = ch.getFreshLabel("cond")
          val afterLabel = ch.getFreshLabel("after")
          ch << Label(condLabel)
          exprToBytecode(ch, expr, symbol, varsMap)
          ch << IfEq(afterLabel)
          statToBytecode(ch, stat, symbol, varsMap)
          ch << Goto(condLabel)
          ch << Label(afterLabel)

        case Println(expr) =>
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          exprToBytecode(ch, expr, symbol, varsMap)
          val methSignature = "(" + getTypeAsString(expr.getType) + ")V"
          ch << InvokeVirtual("java/io/PrintStream", "println", methSignature)

        case Assign(id, expr) =>
          val mSym = symbol match {
            case Some(v: MethodSymbol) => v
            case None                  => sys.error("Internal error")
          }
          mSym.params.get(id.value) match {
            case Some(v) => //Assignment to argument
              val index = mSym.argList.indexOf(v) + 1
              exprToBytecode(ch, expr, symbol, varsMap)
              ch << XStore(index, v.getType)
            case None => mSym.members.get(id.value) match {
              case Some(v) => //Assignment to method var
                exprToBytecode(ch, expr, symbol, varsMap)
                ch << XStore(varsMap(v.name), v.getType)
              case None => mSym.classSymbol.lookupVar(id.value) match {
                case Some(v) => //Assignment to class member
                  ch << ArgLoad(0)
                  exprToBytecode(ch, expr, symbol, varsMap)
                  ch << PutField(mSym.classSymbol.name, id.value, getTypeAsString(v.getType))
                case None => sys.error("Internal error")
              }
            }
          }

        case ArrayAssign(id, index, expr) =>
          exprToBytecode(ch, id, symbol, varsMap)
          exprToBytecode(ch, index, symbol, varsMap)
          exprToBytecode(ch, expr, symbol, varsMap)
          ch << IASTORE
      }
    }

    def exprToBytecode(ch: CodeHandler, expr: ExprTree, symbol: Option[MethodSymbol], varsMap: Map[String, Int]): Unit = expr match {
      case And(lhs, rhs) =>
        val trueLabel = ch.getFreshLabel("true")
        val afterLabel = ch.getFreshLabel("after")
        exprToBytecode(ch, lhs, symbol, varsMap)
        ch << IfNe(trueLabel)
        ch << Ldc(0) << Goto(afterLabel)
        ch << Label(trueLabel)
        exprToBytecode(ch, rhs, symbol, varsMap)
        ch << Label(afterLabel)

      case Or(lhs, rhs) =>
        val falseLabel = ch.getFreshLabel("false")
        val afterLabel = ch.getFreshLabel("after")
        exprToBytecode(ch, lhs, symbol, varsMap)
        ch << IfEq(falseLabel)
        ch << Ldc(1) << Goto(afterLabel)
        ch << Label(falseLabel)
        exprToBytecode(ch, rhs, symbol, varsMap)
        ch << Label(afterLabel)

      case Plus(lhs, rhs) =>
        def concatenateStrings(lhs: ExprTree, rhs: ExprTree, ch: CodeHandler, symbol: Option[MethodSymbol], varsMap: Map[String, Int]) = {
          val (methSign1, methSign2) = (lhs.getType, rhs.getType) match {
            case (TString, TString) =>
              ("(Ljava/lang/String;)Ljava/lang/StringBuilder;", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
            case (TString, TInt) =>
              ("(Ljava/lang/String;)Ljava/lang/StringBuilder;", "(I)Ljava/lang/StringBuilder;")
            case (TInt, TString) =>
              ("(I)Ljava/lang/StringBuilder;", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
            case _ =>
              sys.error("Internal error")
          }
          ch << DefaultNew("java/lang/StringBuilder")
          exprToBytecode(ch, lhs, symbol, varsMap)
          ch << InvokeVirtual("java/lang/StringBuilder", "append", methSign1)
          exprToBytecode(ch, rhs, symbol, varsMap)
          ch << InvokeVirtual("java/lang/StringBuilder", "append", methSign2)
          ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
        }
        (lhs.getType, rhs.getType) match {
          case (TInt, TInt) =>
            exprToBytecode(ch, lhs, symbol, varsMap)
            exprToBytecode(ch, rhs, symbol, varsMap)
            ch << IADD
          case _ => concatenateStrings(lhs, rhs, ch, symbol, varsMap)
        }

      case Minus(lhs, rhs) =>
        exprToBytecode(ch, lhs, symbol, varsMap)
        exprToBytecode(ch, rhs, symbol, varsMap)
        ch << ISUB

      case Times(lhs, rhs) =>
        exprToBytecode(ch, lhs, symbol, varsMap)
        exprToBytecode(ch, rhs, symbol, varsMap)
        ch << IMUL

      case Div(lhs, rhs) =>
        exprToBytecode(ch, lhs, symbol, varsMap)
        exprToBytecode(ch, rhs, symbol, varsMap)
        ch << IDIV

      case LessThan(lhs, rhs) =>
        val lessLabel = ch.getFreshLabel("less")
        val afterLabel = ch.getFreshLabel("after")
        exprToBytecode(ch, lhs, symbol, varsMap)
        exprToBytecode(ch, rhs, symbol, varsMap)
        ch << If_ICmpLt(lessLabel)
        ch << Ldc(0) << Goto(afterLabel)
        ch << Label(lessLabel) << Ldc(1)
        ch << Label(afterLabel)

      case Equals(lhs, rhs) =>
        val equalsLabel = ch.getFreshLabel("equals")
        val afterLabel = ch.getFreshLabel("after")
        exprToBytecode(ch, lhs, symbol, varsMap)
        exprToBytecode(ch, rhs, symbol, varsMap)
        (lhs.getType, rhs.getType) match {
          case (TInt, TInt)         => ch << If_ICmpEq(equalsLabel)
          case (TBoolean, TBoolean) => ch << If_ICmpEq(equalsLabel)
          case (_, _)               => ch << If_ACmpEq(equalsLabel)
        }
        ch << Ldc(0) << Goto(afterLabel)
        ch << Label(equalsLabel) << Ldc(1)
        ch << Label(afterLabel)

      case ArrayRead(arr, index) =>
        exprToBytecode(ch, arr, symbol, varsMap)
        exprToBytecode(ch, index, symbol, varsMap)
        ch << IALOAD

      case ArrayLength(arr) =>
        exprToBytecode(ch, arr, symbol, varsMap)
        ch << ARRAYLENGTH

      case MethodCall(obj, meth, args) =>
        exprToBytecode(ch, obj, symbol, varsMap)
        val className = meth.getSymbol match {
          case MethodSymbol(name, classSymbol) => classSymbol.name
          case _                               => sys.error("Internal error")
        }
        val retType = getTypeAsString(meth.getType)
        val argDec = meth.getSymbol match {
          case m @ MethodSymbol(name, classSymbol) => m.argList
          case _                                   => sys.error("Internal error")
        }
        val argTypes = argDec.map { a => getTypeAsString(a.getType) }.foldRight("")((a, b) => a + b)
        val methSignature = "(" + argTypes + ")" + retType
        args.foreach { a => exprToBytecode(ch, a, symbol, varsMap) }
        ch << InvokeVirtual(className, meth.value, methSignature)

      case IntLit(value) =>
        ch << Ldc(value)

      case StringLit(value) =>
        ch << Ldc(value)

      case True() =>
        ch << Ldc(1)

      case False() =>
        ch << Ldc(0)

      case i @ Identifier(value) =>
        val mSym = symbol match {
          case Some(v: MethodSymbol) => v
          case None                  => sys.error("Internal error")
        }
        mSym.params.get(value) match {
          case Some(v) => //Loading a method argument
            val index = mSym.argList.indexOf(v) + 1
            ch << ArgLoad(index)
          case None => mSym.members.get(value) match {
            case Some(v) => //Loading a method var
              ch << XLoad(varsMap(value), i.getType)
            case None => mSym.classSymbol.lookupVar(value) match {
              case Some(v) => //Loading a class member
                ch << ArgLoad(0)
                ch << GetField(mSym.classSymbol.name, value, getTypeAsString(v.getType))
              case None =>
                sys.error("Internal error")
            }
          }
        }

      case This() =>
        ch << ArgLoad(0)

      case NewIntArray(size) =>
        exprToBytecode(ch, size, symbol, varsMap)
        ch << NewArray(10)

      case New(tpe) =>
        ch << DefaultNew(tpe.value)

      case Not(expr) =>
        val afterLabel = ch.getFreshLabel("after")
        val oneLabel = ch.getFreshLabel("one")
        exprToBytecode(ch, expr, symbol, varsMap)
        ch << IfEq(oneLabel)
        ch << Ldc(0) << Goto(afterLabel)
        ch << Label(oneLabel) << Ldc(1)
        ch << Label(afterLabel)
    }

    def generateMainFile(sourceName: String, main: MainObject, dir: String): Unit = {
      val classFile = new ClassFile(main.id.value, None)
      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor

      val methodHandler = classFile.addMainMethod
      generateMainMethodCode(methodHandler.codeHandler, main.stats, main.id.value)

      classFile.writeToFile((if (dir == "") "./" else dir) + main.id.value + ".class")
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {
      stmts.foreach { s => statToBytecode(ch, s, None, Map()) }
      ch << RETURN
      ch.freeze
    }

    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.getName

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    // Now do the main method
    val main = prog.main
    generateMainFile(sourceName, main, outDir)
  }
}
