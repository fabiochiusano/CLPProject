package toolc
package analyzer

import ast.Trees._

import Symbols._
import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {
  /** Typechecking does not produce a new value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._
    
    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
          case e@And(lhs, rhs) =>
            val (t1, t2) = (tcExpr(lhs, TBoolean), tcExpr(rhs, TBoolean))
            if (t1 == TBoolean && t1 == t2) e.setType(TBoolean) else e.setType(TError)
            e.getType
          case e@Or(lhs, rhs) =>
            val (t1, t2) = (tcExpr(lhs, TBoolean), tcExpr(rhs, TBoolean))
            if (t1 == TBoolean && t1 == t2) e.setType(TBoolean) else e.setType(TError)
            e.getType
          case e@Plus(lhs, rhs) => 
            val (t1, t2) = (tcExpr(lhs, TInt, TString), tcExpr(rhs, TInt, TString))
            val tf =
              if (t1 == TInt && t2 == TInt) TInt
              else if (t1 == TInt && t2 == TString) TString
              else if (t1 == TString && t2 == TInt) TString
              else if (t1 == TString && t2 == TString) TString
              else TError
            e.setType(tf)
            e.getType
          case e@Minus(lhs, rhs) =>
            val (t1, t2) = (tcExpr(lhs, TInt), tcExpr(rhs, TInt))
            if (t1 == TInt && t1 == t2) e.setType(TInt) else e.setType(TError)
            e.getType
          case e@Times(lhs, rhs) =>
            val (t1, t2) = (tcExpr(lhs, TInt), tcExpr(rhs, TInt))
            if (t1 == TInt && t1 == t2) e.setType(TInt) else e.setType(TError)
            e.getType
          case e@Div(lhs, rhs) =>
            val (t1, t2) = (tcExpr(lhs, TInt), tcExpr(rhs, TInt))
            if (t1 == TInt && t1 == t2) e.setType(TInt) else e.setType(TError)
            e.getType
          case e@LessThan(lhs, rhs) =>
            val (t1, t2) = (tcExpr(lhs, TInt), tcExpr(rhs, TInt))
            if (t1 == TInt && t1 == t2) e.setType(TBoolean) else e.setType(TError)
            e.getType
          case e@Equals(lhs, rhs) =>
            val (t1, t2) = (tcExpr(lhs, TInt, TBoolean, TString, TIntArray, TAnyObject), tcExpr(rhs, TInt, TBoolean, TString, TIntArray, TAnyObject))
            val tf = (t1, t2) match {
              case (TInt, TInt) => TBoolean
              case (TString, TString) => TBoolean
              case (TBoolean, TBoolean) => TBoolean
              case (TIntArray, TIntArray) => TBoolean
              case (TObject(_), TObject(_)) => TBoolean
              case _ => TError
            }
            e.setType(tf)
            e.getType
          case e@ArrayRead(arr, index) =>
            val (t1, t2) = (tcExpr(arr, TIntArray), tcExpr(index, TInt))
            if (t1 == TIntArray && t2 == TInt) e.setType(TInt) else e.setType(TError)
            e.getType
          case e@ArrayLength(arr) =>
            val t1 = tcExpr(arr, TIntArray)
            if (t1 == TIntArray) e.setType(TInt) else e.setType(TError)
            e.getType
          case e@MethodCall(obj, meth, args) =>
            tcExpr(obj, TAnyObject) match {
              case c@TObject(_) => {
                c.classSymbol.lookupMethod(meth.value) match {
                  case Some(m) =>
                    meth.setSymbol(m)
                    val paramList = m.argList
                    if (args.length == paramList.length && args.forall { arg => {
                        val par = paramList(args.indexOf(arg))
                        tcExpr(arg, par.getType).isSubTypeOf(par.getType)
                      }
                    }) e.setType(m.getType) else e.setType(TError)
                  case _ => e.setType(TError)
                }
              }
              case _ => e.setType(TError);
            }
            e.getType
          case e@IntLit(value) =>
            e.setType(TInt)
            e.getType
          case e@StringLit(value) =>
            e.setType(TString)
            e.getType
          case e@True() =>
            e.setType(TBoolean)
            e.getType
          case e@False() =>
            e.setType(TBoolean)
            e.getType
          case e@Identifier(value) => e.getType
          case e@This() => e.getSymbol.getType
          case e@NewIntArray(size) =>
            val t1 = tcExpr(size, TInt)
            if (t1 == TInt) e.setType(TIntArray) else e.setType(TError)
            e.getType
          case e@New(tpe) => e.setType(tpe.getType); e.getType
          case e@Not(expr) =>
            val t1 = tcExpr(expr, TBoolean)
            if (t1 == TBoolean) e.setType(TBoolean) else e.setType(TError)
            e.getType
      }
      
      if (tpe == TError) error ("Error in typechecking expression (" + expected.toList.mkString(" or ") + "): ", expr)

      // Check result and return a valid type in case of error
      if(expected.isEmpty) {
        tpe
      } else {
        if(!expected.exists(e => tpe.isSubTypeOf(e))) {
          error("Type error: Expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
          expected.head
        } else {
          tpe
        }
      }
    }

    def tcStat(stat: StatTree): Unit = stat match {
      case Block(stats) => stats.foreach(tcStat(_)) 
      case If(expr, thn, els) => tcExpr(expr, TBoolean); tcStat(thn); els match {case Some(v) => tcStat(v); case None => }
      case While(expr, stat) => tcExpr(expr, TBoolean); tcStat(stat)
      case Println(expr) => tcExpr(expr, TInt, TBoolean, TString)
      case Assign(id, expr) => tcExpr(expr, id.getSymbol.getType)
      case ArrayAssign(id, index, expr) => tcExpr(id, TIntArray); tcExpr(index, TInt); tcExpr(expr, TInt)
    }
    
    // Traverse and typecheck the program
    def tc(t: Tree): Unit = t match {
      case Program(main, classes) => tc(main); classes.foreach(tc(_))
      case MainObject(id, stats) => stats.foreach(tcStat(_))
      case ClassDecl(id, parent, vars, methods, nativeMeth) => methods.foreach(tc(_))
      case m @ MethodDecl(retType, id, args, vars, stats, retExpr) => stats.foreach(tcStat(_)); tcExpr(retExpr, m.getSymbol.getType)
      case _ => 
    }
    
    tc(prog)
    
    prog
  }
}
