package forest

import scala.js._
import scala.virtualization.lms.common._
import scala.reflect.SourceContext

// --- Case classes support

/*trait CaseClasses extends Base {
  case class CaseClass[A](a: Rep[A]) extends Struct[Rep] {
    def selectDynamic[B : Manifest](n: String): Rep[B] = field[A, B](a, n)
  }
  def asCaseClass[A](a: Rep[A]): CaseClass[A] = CaseClass[A](a)
  def field[A, B : Manifest](a: Rep[A], n: String): Rep[B]
}

trait CaseClassesInScala extends CaseClasses with JSInScala {
  def field[A, B : Manifest](a: A, n: String): B = a.getClass.getField(n).get(a).asInstanceOf[B]
}

trait CaseClassesExp extends BaseExp with CaseClasses {

  case class Field[A, B : Manifest](target: Exp[A], name: String) extends Def[B]

  override def field[A, B : Manifest](a: Exp[A], n: String): Exp[B] = toAtom(Field(a, n))

}

trait JSGenCaseClasses extends JSGenBase {
  val IR: CaseClassesExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Field(o, n) => emitValDef(sym, quote(o) + "." + n)
    case _ => super.emitNode(sym, rhs)
  }

}

trait ScalaGenCaseClasses extends ScalaGenBase {
  val IR: CaseClassesExp
  import IR.__

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Field(o, n) => emitValDef(sym, quote(o) + "." + n)
    case _ => super.emitNode(sym, rhs)
  }

}*/

// TODO Do something with macros, or unify with JSProxy
trait Fields extends Base { fields =>

  class Fields[A] {
    def field[B](name: String)(implicit target: Rep[A]): Rep[B] = fields.field(target, name)
  }
  
  def field[A, B : Manifest](target: Rep[A], name: String): Rep[B]
}

trait FieldsExp extends Fields with BaseExp {
  case class FieldsField[A, B : Manifest](target: Exp[A], name: String) extends Def[B]
  override def field[A, B : Manifest](target: Rep[A], name: String): Rep[B] = toAtom(FieldsField(target, name)) // FIXME Why do I need to write toAtom explicitly?
}

trait JSGenFields extends JSGenBase {
  val IR: FieldsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case FieldsField(o, n) => emitValDef(sym, quote(o) + "." + n)
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenFields extends ScalaGenBase {
  val IR: FieldsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case FieldsField(o, n) => emitValDef(sym, quote(o) + "." + n)
    case _ => super.emitNode(sym, rhs)
  }
}


// --- List support

trait ListOpsExpOpt extends ListOpsExp {
  override def list_concat[A : Manifest](xs1: Exp[List[A]], xs2: Exp[List[A]])(implicit pos: SourceContext): Exp[List[A]] = (xs1, xs2) match {
    case (Def(ListNew(xs1)), Def(ListNew(xs2))) => ListNew(xs1 ++ xs2)
    case _ => super.list_concat(xs1, xs2)
  }
}


// --- JSProxy support

trait ScalaGenProxy extends ScalaGenBase with ScalaGenEffect {
  val IR: JSProxyExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {

  case MethodCall(receiver, method, args) =>
    emitValDef(sym, quote(receiver) + "." + method + args.map(quote).mkString("(", ", ", ")"))

  case SuperMethodCall(receiver, _, method, args) =>
    emitValDef(sym, "super." + method + args.map(quote).mkString("(", ", ", ")"))

  case FieldAccess(receiver, field) =>
    emitValDef(sym, quote(receiver) + "." + field)

  case FieldUpdate(receiver, field, value) =>
    emitValDef(sym, quote(receiver) + "." + field + " = " + quote(value))

  case _ => super.emitNode(sym, rhs)
  }

}
