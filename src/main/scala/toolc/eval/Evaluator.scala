package toolc
package eval

import ast.Trees._
import utils._

class Evaluator(ctx: Context, prog: Program) {
  import ctx.reporter._

  def eval() {
    // Initialize the context for the main method
    val ectx = new MainMethodContext

    // Evaluate each statement of the main method
    prog.main.stats.foreach(evalStatement(ectx, _))
  }

  def evalStatement(ectx: EvaluationContext, stmt: StatTree): Unit = stmt match {
    case Block(stats) => stats.foreach(evalStatement(ectx, _))
    case If(expr, thn, els) => if(evalExpr(ectx, expr) == BoolValue(true)) evalStatement(ectx, thn)
                               else if (els != None) evalStatement(ectx, els.get)
    case While(expr, stat) => while(evalExpr(ectx, expr) == BoolValue(true)) evalStatement(ectx, stat)
    case Println(expr) => evalExpr(ectx, expr) match {
      case IntValue(value) => println(value)
      case StringValue(value) => println(value)
      case BoolValue(value) => println(value)
      case other => fatal("cannot print "+other)
    }
    case Assign(id, expr) => ectx.setVariable(id.value, evalExpr(ectx, expr))
    case ArrayAssign(id, index, expr) => 
      ectx.getVariable(id.value).asArray.setIndex(evalExpr(ectx, index).asInt, evalExpr(ectx, expr).asInt)
    case _ =>
      fatal("unnexpected statement", stmt)
  }

  def evalExpr(ectx: EvaluationContext, e: ExprTree): Value = e match {
    case IntLit(value)    => IntValue(value)
    case StringLit(value) => StringValue(value)
    case True()           => BoolValue(true)
    case False()          => BoolValue(false)    
    case And(lhs, rhs) => BoolValue(evalExpr(ectx, lhs).asBool && evalExpr(ectx, rhs).asBool)
    case Or(lhs, rhs)  => BoolValue(evalExpr(ectx, lhs).asBool || evalExpr(ectx, rhs).asBool)
    case Plus(lhs, rhs) => (evalExpr(ectx, lhs), evalExpr(ectx, rhs)) match {
      case (IntValue(x), IntValue(y)) => IntValue(x + y)
      case (IntValue(x), StringValue(y)) => StringValue(x + y)
      case (StringValue(x), IntValue(y)) => StringValue(x + y)
      case (StringValue(x), StringValue(y)) => StringValue(x + y)
      case (_, _) => fatal("illegal plus")
    } 
    case Minus(lhs, rhs) => (evalExpr(ectx, lhs), evalExpr(ectx, rhs)) match {
      case (IntValue(x), IntValue(y)) => IntValue(x - y)
      case (_, _) => fatal("illegal minus")
    }
    case Times(lhs, rhs) => (evalExpr(ectx, lhs), evalExpr(ectx, rhs)) match {
      case (IntValue(x), IntValue(y)) => IntValue(x * y)
      case (_, _) => fatal("illegal times")
    }
    case Div(lhs, rhs) => (evalExpr(ectx, lhs), evalExpr(ectx, rhs)) match {
      case (IntValue(x), IntValue(0)) => fatal("fatal: zero division not possible")
      case (IntValue(x), IntValue(y)) => IntValue(x / y)
      case (_, _) => fatal("illegal minus")
    }
    case LessThan(lhs, rhs) => (evalExpr(ectx, lhs), evalExpr(ectx, rhs)) match {
      case (IntValue(x), IntValue(y)) => BoolValue(x < y)
      case (_, _) => fatal("illegal less")
    }
    case Not(expr) => BoolValue(!evalExpr(ectx, expr).asBool)
    case Equals(lhs, rhs) =>
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)
      val res = (lv, rv) match {
        case (IntValue(l), IntValue(r)) => l == r
        case (BoolValue(l), BoolValue(r)) => l == r
        case (lr, rr) => lr eq rr
      }
      BoolValue(res)

    case ArrayRead(arr, index) => IntValue(evalExpr(ectx, arr).asArray.getIndex(evalExpr(ectx, index).asInt))
    case ArrayLength(arr) => IntValue(evalExpr(ectx, arr).asArray.size)
    case MethodCall(obj, meth, args) => {
      val objValue = evalExpr(ectx, obj).asObject
      val clas = findClass(objValue.cd.id.value)
      val method = findMethod(clas, meth.value)
      val mctx = new MethodContext(objValue)
      method.vars.foreach(x => mctx.declareVariable(x.id.value))
      method.args.foreach(x => {mctx.declareVariable(x.id.value);
                                mctx.setVariable(x.id.value, evalExpr(ectx, args(method.args.indexOf(x))))})
      method.stats.foreach(evalStatement(mctx, _))
      evalExpr(mctx, method.retExpr)
    }
    case Identifier(name) => ectx.getVariable(name)
    case New(tpe) => {
      val clas = findClass(tpe.value)
      val obj = ObjectValue(clas)
      fieldsOfClass(clas).foreach(obj.declareField(_))
      obj
    }
    case This() => ectx.asMethodContext().obj
    case NewIntArray(size) => ArrayValue(new Array[Int](evalExpr(ectx, size).asInt), evalExpr(ectx, size).asInt)
  }

  // Define the scope of evaluation, with methods to access/declare/set local variables(or arguments)
  abstract class EvaluationContext {
    def getVariable(name: String): Value
    def setVariable(name: String, v: Value): Unit
    def declareVariable(name: String): Unit
    def asMethodContext(): MethodContext = fatal("Not in a method context")
  }

  // A Method context consists of the execution context within an object method.
  // getVariable can fallback to the fields of the current object
  class MethodContext(val obj: ObjectValue) extends EvaluationContext {
    var vars = Map[String, Option[Value]]()

    def getVariable(name: String): Value = {
      vars.get(name) match {
        case Some(ov) =>
          ov.getOrElse(fatal("Uninitialized variable '"+name+"'"))
        case _ =>
          obj.getField(name)
      }
    }

    def setVariable(name: String, v: Value) {
      if (vars contains name) {
        vars += name -> Some(v)
      } else {
        obj.setField(name, v)
      }
    }

    def declareVariable(name: String) {
      vars += name -> None
    }
    
    override def asMethodContext(): MethodContext = this
  }

  // Special execution context for the main method, which is very limited.
  class MainMethodContext extends EvaluationContext {
    def getVariable(name: String): Value          = fatal("The main method contains no variable and/or field! .."+name)
    def setVariable(name: String, v: Value): Unit = fatal("The main method contains no variable and/or field! .."+name)
    def declareVariable(name: String): Unit       = fatal("The main method contains no variable and/or field! .."+name)
  }

  // Helper functions to query the current program
  def findMethod(cd: ClassDecl, name: String): MethodDecl = {
    cd.methods.find(_.id.value == name).orElse(
      cd.parent.map(p => findMethod(findClass(p.value), name))
    ).getOrElse(fatal("Unknown method "+cd.id+"."+name))
  }

  def findClass(name: String): ClassDecl = {
    prog.classes.find(_.id.value == name).getOrElse(fatal("Unknown class '"+name+"'"))
  }

  def fieldsOfClass(cl: ClassDecl): Set[String] = {
    cl.vars.map(_.id.value).toSet ++
      cl.parent.map(p => fieldsOfClass(findClass(p.value))).getOrElse(Set())
  }

  // Runtime evaluation values, with as* methods which act as typecasts for convenience.
  sealed abstract class Value {
    def asInt: Int            = fatal("Unnexpected value, found "+this+" expected Int")
    def asString: String      = fatal("Unnexpected value, found "+this+" expected String")
    def asBool: Boolean       = fatal("Unnexpected value, found "+this+" expected Boolean")
    def asObject: ObjectValue = fatal("Unnexpected value, found "+this+" expected Object")
    def asArray: ArrayValue   = fatal("Unnexpected value, found "+this+" expected Array")
  }

  case class ObjectValue(cd: ClassDecl) extends Value {
    var fields = Map[String, Option[Value]]()

    def setField(name: String, v: Value) {
      if (fields contains name) {
        fields += name -> Some(v)
      } else {
        fatal("Unknown field '"+name+"'")
      }
    }

    def getField(name: String) = {
      fields.get(name).flatten.getOrElse(fatal("Unknown field '"+name+"'"))
    }

    def declareField(name: String) {
      fields += name -> None
    }

    override def asObject = this
  }

  case class ArrayValue(var entries: Array[Int], val size: Int) extends Value {
    def setIndex(i: Int, v: Int) {
      if (i >= size || i < 0) {
        fatal("Index '"+i+"' out of bounds (0 .. "+size+")")
      }
      entries(i) = v
    }

    def getIndex(i: Int) = {
      if (i >= size || i < 0) {
        fatal("Index '"+i+"' out of bounds (0 .. "+size+")")
      }
      entries(i)
    }

    override def asArray = this
  }

  case class StringValue(var v: String) extends Value {
    override def asString = v
  }

  case class IntValue(var v: Int) extends Value {
    override def asInt = v
  }

  case class BoolValue(var v: Boolean) extends Value {
    override def asBool = v
  }
}

