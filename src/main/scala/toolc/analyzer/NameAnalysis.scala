package toolc
package analyzer

import utils._
import ast.Trees._
import Symbols._
import Types._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    val globalScope = new GlobalScope

    def setSymbols(tree: Tree, currentScope: Symbol): Unit = tree match {
      case Program(main, classes) =>
        val mainSymbol = (new ClassSymbol(main.id.value)).setPos(main)
        mainSymbol.setType(TObject(mainSymbol))
        main.setSymbol(mainSymbol)
        globalScope.mainClass = mainSymbol
        setSymbols(main, mainSymbol)
        classes.foreach { x => val sym = new ClassSymbol(x.id.value).setPos(x); sym.setType(TObject(sym)); x.setSymbol(sym) }
        val classSymbols = classes map { x => x.getSymbol }
        classSymbols.groupBy { x => x.name }.foreach { x =>
          if (x._2.length > 1) error("Class " + x._1 +
            "defined more that once", x._2.head)
        }
        classSymbols.foreach { x =>
          if (x.name == mainSymbol.name) error("Class " + x.name + " has same " +
            "name of main object", x)
        }
        globalScope.classes = classSymbols.map(x => (x.name, x)).toMap
        classes.foreach { x => setSymbols(x, x.getSymbol) }

      case MainObject(id, stats) =>
        val mainSymbol = globalScope.mainClass
        id.setSymbol(mainSymbol)

      case ClassDecl(id, parent, vars, method, nativeMeth) =>
        val classSymbol = currentScope match {
          case c: ClassSymbol => c
          case _              => sys.error("Internal error")
        }
        id.setSymbol(classSymbol)
        parent match {
          case Some(id) =>
            globalScope.classes.get(id.value) match {
              case Some(sym) =>
                id.setSymbol(sym)
                classSymbol.parent = Some(sym)
              case None => error("class " + id + " not defined", id)
            }
          case None => None
        }
        vars.foreach { x => val sym = new VariableSymbol(x.id.value).setPos(x); sym.setType(getTypeFromNode(x)); x.setSymbol(sym) }
        val varSymbols = vars map (x => x.getSymbol)
        varSymbols.groupBy { x => x.name }.foreach { x =>
          if (x._2.length > 1) error("Variable " + x._1 +
            " is defined more than once", x._2.head)
        }
        classSymbol.members = varSymbols.map(x => (x.name, x)).toMap
        vars.foreach { x => setSymbols(x, classSymbol) }
        method.foreach { x => val sym = new MethodSymbol(x.id.value, classSymbol).setPos(x); sym.setType(getTypeFromNode(x)); x.setSymbol(sym) }
        val methodSymbols = method map (x => x.getSymbol)
        methodSymbols.groupBy { x => x.name }.foreach { x =>
          if (x._2.length > 1) error("Method " + x._1 +
            " is defined more than once", x._2.head)
        }
        classSymbol.methods = methodSymbols.map(x => (x.name, x)).toMap

        //Custom extension: look for native methods
        nativeMeth match {
          case Some(list) =>
            list.foreach { x => val sym = new MethodSymbol(x.id.value, classSymbol).setPos(x); sym.setType(getTypeFromNode(x)); x.setSymbol(sym) }
            classSymbol.methods ++= list.map(x => x.getSymbol).map(x => (x.name, x)).toMap
            list.foreach { x => setSymbols(x, classSymbol) }
          case None =>
        }

        if (hasInheritanceCycle(classSymbol, List())) error("Inheritance cycle", classSymbol)
        method.foreach { x => setSymbols(x, classSymbol) }

        def hasInheritanceCycle(c: ClassSymbol, prev: List[ClassSymbol]): Boolean = c.parent match {
          case None    => false
          case Some(p) => prev.contains(p) || hasInheritanceCycle(p, c :: prev)
        }

      case v @ VarDecl(tpe, id) =>
        id.setSymbol(v.getSymbol)

      case NativeMethodDecl(ret, id, args) =>
        val classScope = currentScope match {
          case c: ClassSymbol => c
          case _              => sys.error("Internal error")
        }
        classScope.methods.get(id.value) match {
          case Some(m) => setNativeMethod(m)
          case None => classScope.parent match {
            case Some(p) =>
              p.methods.get(id.value) match {
                case Some(m) => setNativeMethod(m)
                case None    => error("method " + id + " undeclared", id)
              }
            case None => error("method " + id + " undeclared", id)
          }
        }
        def setNativeMethod(methodSymbol: MethodSymbol) = {
          id.setSymbol(methodSymbol)
          args.foreach { x => val sym = new VariableSymbol(x.id.value).setPos(x); sym.setType(getTypeFromNode(x)); x.setSymbol(sym) }
          val paramSymbols = args map (x => x.getSymbol)
          methodSymbol.argList = paramSymbols
          methodSymbol.params = paramSymbols.map(x => (x.name, x)).toMap
          args.foreach { x => setSymbols(x, methodSymbol) }
        }

      case MethodDecl(ret, id, args, vars, stats, retExpr) =>
        val classScope = currentScope match {
          case c: ClassSymbol => c
          case _              => sys.error("Internal error")
        }
        classScope.methods.get(id.value) match {
          case Some(m) => setMethod(m)
          case None => classScope.parent match {
            case Some(p) =>
              p.methods.get(id.value) match {
                case Some(m) => setMethod(m)
                case None    => error("method " + id + " undeclared", id)
              }
            case None => error("method " + id + " undeclared", id)
          }
        }
        def setMethod(methodSymbol: MethodSymbol) = {
          id.setSymbol(methodSymbol)
          args.foreach { x => val sym = new VariableSymbol(x.id.value).setPos(x); sym.setType(getTypeFromNode(x)); x.setSymbol(sym) }
          val paramSymbols = args map (x => x.getSymbol)
          methodSymbol.argList = paramSymbols
          methodSymbol.params = paramSymbols.map(x => (x.name, x)).toMap
          args.foreach { x => setSymbols(x, methodSymbol) }
          vars.foreach { x => val sym = new VariableSymbol(x.id.value).setPos(x); sym.setType(getTypeFromNode(x)); x.setSymbol(sym) }
          val varSymbols = vars map (x => x.getSymbol)
          (varSymbols ++ paramSymbols).groupBy { x => x.name }.foreach { x =>
            if (x._2.length > 1)
              error("Variable " + x._1 + " is defined more than once", x._2.head)
          }
          methodSymbol.members = varSymbols.map(x => (x.name, x)).toMap
          vars.foreach { x => setSymbols(x, methodSymbol) }
        }

      case f @ Formal(tpe, id) =>
        id.setSymbol(f.getSymbol)

      case _ =>
    }

    def getTypeFromNode(node: Tree): Type = node match {
      case VarDecl(tpe, id) => getTypeFromNode(tpe)
      case MethodDecl(retType, id, args, vars, stats, retExpr) => getTypeFromNode(retType)
      case NativeMethodDecl(retType, id, args) => getTypeFromNode(retType)
      case Formal(tpe, id) => getTypeFromNode(tpe)
      case IntType() => TInt
      case BooleanType() => TBoolean
      case StringType() => TString
      case IntArrayType() => TIntArray
      case i @ Identifier(str) =>
        globalScope.lookupClass(str) match { case Some(c) => TObject(c); case None => TError }
      case _ => sys.error("Internal error")
    }

    def findOverriddenClassMembers(classes: List[ClassSymbol]): Unit = if (!classes.isEmpty) {
      classes.head.parent match {
        case None =>
        case Some(p) => classes.head.members.foreach { x =>
          p.lookupVar(x._1) match {
            case Some(_) => error("Variable " + x._1 + " cannot be overridden", x._2)
            case None    =>
          }
        }
      }
      findOverriddenClassMembers(classes.tail)
    }

    def findOverriddenClassMethods(classes: List[ClassSymbol]): Unit = if (!classes.isEmpty) {
      classes.head.parent match {
        case None =>
        case Some(p) => classes.head.methods.foreach { x =>
          p.lookupMethod(x._1) match {
            case Some(m) => checkMethodOverriding(m, x._2)
            case None    =>
          }
        }
      }
      findOverriddenClassMethods(classes.tail)
    }

    def checkMethodOverriding(mOld: MethodSymbol, mNew: MethodSymbol) = {
      def checkParameterTypes(pOld: List[VariableSymbol], pNew: List[VariableSymbol], mNew: MethodSymbol): Unit = (pOld, pNew) match {
        case (Nil, Nil) =>
        case (x :: xs, y :: ys) =>
          if (x.getType == y.getType) checkParameterTypes(xs, ys, mNew)
          else error("Method cannot be overridden because its parameter types are not good", mNew)
        case _ => sys.error("Internal error")
      }
      if (mOld.argList.length == mNew.argList.length) {
        checkParameterTypes(mOld.argList, mNew.argList, mNew)
        if (mOld.getType != mNew.getType) error("Method cannot be overridden because its return type is wrong", mNew)
      } else
        error("Method cannot be overridden because its number of parameters is wrong", mNew)
    }

    def attachSymbols(tree: Tree, currentScope: Symbol): Unit = tree match {
      case Program(main, classes) =>
        attachSymbols(main, main.getSymbol)
        classes.foreach { x => attachSymbols(x, x.getSymbol) }

      case MainObject(id, stats) =>
        stats.foreach { x => attachSymbols(x, currentScope) }

      case ClassDecl(id, parent, vars, methods, nativeMeth) =>
        methods.foreach { x => attachSymbols(x, currentScope) }
        vars.foreach { x => attachSymbols(x, currentScope) }

        //Custom extension: attach symbols to native methods
        nativeMeth match {
          case Some(list) => list.foreach { x => attachSymbols(x, currentScope) }
          case None       =>
        }

      //Custom extension: attach symbols to native methods
      case NativeMethodDecl(retType, id, args) =>
        val classSymbol = currentScope match {
          case c: ClassSymbol => c
          case _              => sys.error("Internal error")
        }
        args.foreach { x => attachSymbols(x, currentScope) }
        retType match {
          case id: Identifier => globalScope.classes.get(id.value) match {
            case Some(c) => id.setSymbol(c)
            case None    => error("undefined type " + id, id)
          }
          case _ =>
        }

      case MethodDecl(retType, id, args, vars, stats, retExpr) =>
        val classSymbol = currentScope match {
          case c: ClassSymbol => c
          case _              => sys.error("Internal error")
        }
        classSymbol.methods.get(id.value) match {
          case Some(methodScope) =>
            stats.foreach { x => attachSymbols(x, methodScope) }
            attachSymbols(retExpr, methodScope)
          case None => error("method " + id + " undeclared", id)
        }
        vars.foreach { x => attachSymbols(x, currentScope) }
        args.foreach { x => attachSymbols(x, currentScope) }
        retType match {
          case id: Identifier => globalScope.classes.get(id.value) match {
            case Some(c) => id.setSymbol(c)
            case None    => error("undefined type " + id, id)
          }
          case _ =>
        }

      case VarDecl(tpe, id) =>
        tpe match {
          case id: Identifier => globalScope.classes.get(id.value) match {
            case Some(c) => id.setSymbol(c)
            case None    => error("undefined type " + id, id)
          }
          case _ =>
        }

      case Formal(tpe, id) =>
        tpe match {
          case id: Identifier => globalScope.classes.get(id.value) match {
            case Some(c) => id.setSymbol(c)
            case None    => error("undefined type " + id, id)
          }
          case _ =>
        }

      case Block(stats) =>
        stats.foreach { x => attachSymbols(x, currentScope) }

      case If(cond, thn, els) =>
        attachSymbols(cond, currentScope)
        attachSymbols(thn, currentScope)
        els match {
          case Some(s) => attachSymbols(s, currentScope)
          case None    =>
        }

      case While(expr, stat) =>
        attachSymbols(expr, currentScope)
        attachSymbols(stat, currentScope)

      case Println(expr) =>
        attachSymbols(expr, currentScope)

      case Assign(id, expr) =>
        attachSymbols(id, currentScope)
        attachSymbols(expr, currentScope)

      case ArrayAssign(id, index, expr) =>
        attachSymbols(id, currentScope)
        attachSymbols(index, currentScope)
        attachSymbols(expr, currentScope)

      case And(l, r) =>
        attachSymbols(l, currentScope)
        attachSymbols(r, currentScope)

      case Or(l, r) =>
        attachSymbols(l, currentScope)
        attachSymbols(r, currentScope)

      case Plus(l, r) =>
        attachSymbols(l, currentScope)
        attachSymbols(r, currentScope)

      case Minus(l, r) =>
        attachSymbols(l, currentScope)
        attachSymbols(r, currentScope)

      case Times(l, r) =>
        attachSymbols(l, currentScope)
        attachSymbols(r, currentScope)

      case Div(l, r) =>
        attachSymbols(l, currentScope)
        attachSymbols(r, currentScope)

      case LessThan(l, r) =>
        attachSymbols(l, currentScope)
        attachSymbols(r, currentScope)

      case Equals(l, r) =>
        attachSymbols(l, currentScope)
        attachSymbols(r, currentScope)

      case ArrayRead(arr, index) =>
        attachSymbols(arr, currentScope)
        attachSymbols(index, currentScope)

      case ArrayLength(arr) =>
        attachSymbols(arr, currentScope)

      case MethodCall(obj, meth, args) =>
        args.foreach { x => attachSymbols(x, currentScope) }
        attachSymbols(obj, currentScope)
        meth.setSymbol(UnresolvedSymbol)

      case t @ This() =>
        currentScope match {
          case m: MethodSymbol =>
            t.setSymbol(m.classSymbol)
            t.setType(m.classSymbol.getType)
          case c: ClassSymbol => error("Cannot reference this from main method", tree)
          case _              => sys.error("Internal error")
        }

      case New(id) =>
        attachSymbols(id, currentScope)

      case NewIntArray(size) =>
        attachSymbols(size, currentScope)

      case Not(e) =>
        attachSymbols(e, currentScope)

      case i @ Identifier(id) =>
        currentScope match {
          case classScope: ClassSymbol =>
            globalScope.classes.get(id) match {
              case Some(c) => i.setSymbol(c)
              case None    => error("undeclared identifier " + id, tree)
            }
          case methodScope: MethodSymbol =>
            methodScope.lookupVar(id) match {
              case Some(c) => i.setSymbol(c)
              case None => globalScope.classes.get(id) match {
                case Some(c) => i.setSymbol(c)
                case None    => error("undeclared identifier " + id, tree)
              }
            }
          case _ =>
        }

      case _ =>
    }

    setSymbols(prog, null)
    findOverriddenClassMembers(globalScope.classes.values.toList)
    findOverriddenClassMethods(globalScope.classes.values.toList)
    attachSymbols(prog, null)

    prog
  }
}
