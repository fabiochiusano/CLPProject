package toolc
package analyzer

import Symbols._

object Types {
  trait Typed {
    self =>

    private var _tpe: Type = TUntyped

    def setType(tpe: Type): self.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _ => false
    }
    override def toString = "int"
  }
  
  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case _ => false
    }
    override def toString = "boolean"
  }
  
  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _ => false
    }
    override def toString = "string"
  }
  
  case object TIntArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TIntArray => true
      case _ => false
    }
    override def toString = "intArray"
  }

  case class TObject(classSymbol: ClassSymbol) extends Type {
    def isSubclassOf(cl1: ClassSymbol, cl2: ClassSymbol): Boolean = cl1 == cl2 || (cl1.parent match {
      case Some(pSym) => if (pSym == cl2) true else isSubclassOf(pSym, cl2)
      case None => false
    })
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TObject(clSym) => isSubclassOf(classSymbol, clSym)
      case TAnyObject => true
      case _ => false
    }
    override def toString = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses
  case object TAnyObject extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TAnyObject => true
      case _ => false
    }
    override def toString = "Object"
  }
}
