package toolc
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = t match {
    case Program(main, classes) => apply(main) + "\n" +
      { for (i <- classes) yield apply(i) }.foldLeft("")((a, b) => a + b)

    case MainObject(id, stats) => "\nobject " + apply(id) + " { def main ( ) : Unit = {\n" +
      { for (i <- stats) yield apply(i) }.foldLeft("")((a, b) => a + b) + "} }"

    case ClassDecl(id, Some(p), vars, meths, _) => "\nclass " + apply(id) + " extends " + apply(p) + " {\n" +
      { for (i <- vars) yield apply(i) }.foldLeft("")((a, b) => a + b) +
      { for (i <- meths) yield apply(i) }.foldLeft("")((a, b) => a + b) + "}"

    case ClassDecl(id, None, vars, meths, _) => "\nclass " + apply(id) + " {\n" +
      { for (i <- vars) yield apply(i) }.foldLeft("")((a, b) => a + b) +
      { for (i <- meths) yield apply(i) }.foldLeft("")((a, b) => a + b) + "}"

    case VarDecl(t, id) => "var " + apply(id) + ": " + apply(t) + ";\n"

    case MethodDecl(retType, id, args, vars, stats, retExpr) => "\ndef " + apply(id) + " (" +
      { for (i <- args) yield apply(i) }.foldLeft("")((a, b) => if (a == "") a + b else a + "," + b) +
      "): " + apply(retType) + " = {\n" +
      { for (i <- vars) yield apply(i) }.foldLeft("")((a, b) => a + b) +
      { for (i <- stats) yield apply(i) }.foldLeft("")((a, b) => a + b) +
      "return " + apply(retExpr) + ";\n}\n"

    case Formal(t, id)                => apply(id) + ": " + apply(t)
    case IntArrayType()               => "Int[]"
    case IntType()                    => "Int"
    case BooleanType()                => "Bool"
    case StringType()                 => "String"
    case Block(stats)                 => "{\n" + { for (i <- stats) yield apply(i) }.foldLeft("")((a, b) => a + b) + "\n}"
    case If(expr, thn, Some(e))       => "if (" + apply(expr) + ")\n" + apply(thn) + "else\n" + apply(e)
    case If(expr, thn, None)          => "if (" + apply(expr) + ")\n" + apply(thn)
    case While(expr, stats)           => "while (" + apply(expr) + ")\n" + apply(stats)
    case Println(e)                   => "println(" + apply(e) + ");\n"
    case Assign(id, expr)             => apply(id) + " = " + apply(expr) + ";\n"
    case ArrayAssign(id, index, expr) => apply(id) + "[" + apply(index) + "] = " + apply(expr) + ";\n"

    case And(l, r)                    => "(" + apply(l) + " && " + apply(r) + ")"
    case Or(l, r)                     => "(" + apply(l) + " || " + apply(r) + ")"
    case Plus(l, r)                   => "(" + apply(l) + " + " + apply(r) + ")"
    case Minus(l, r)                  => "(" + apply(l) + " - " + apply(r) + ")"
    case Times(l, r)                  => "(" + apply(l) + " * " + apply(r) + ")"
    case Div(l, r)                    => "(" + apply(l) + " / " + apply(r) + ")"
    case LessThan(l, r)               => "(" + apply(l) + " < " + apply(r) + ")"
    case Equals(l, r)                 => "(" + apply(l) + " == " + apply(r) + ")"

    case ArrayRead(arr, i)            => apply(arr) + "[" + apply(i) + "]"
    case ArrayLength(arr)             => apply(arr) + ".length"
    case MethodCall(obj, id, args) => apply(obj) + "." + apply(id) + "(" +
      { for (i <- args) yield apply(i) }.foldLeft("")((a, b) => if (a == "") a + b else a + "," + b) + ")"
    case IntLit(n)         => n.toString
    case StringLit(s)      => "\"" + s + "\""
    case True()            => "true"
    case False()           => "false"
    case Identifier(s)     => s + "#" + t.asInstanceOf[Identifier].getSymbol.id
    case This()            => "this"
    case NewIntArray(size) => "new Int [" + apply(size) + "]"
    case New(id)           => "new " + apply(id) + "()"
    case Not(e)            => "!" + apply(e)

    case _                 => "Case not found"
  }
}
