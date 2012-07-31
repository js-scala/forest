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

trait FieldsInScala extends Fields with JSInScala {
  def field[A, B : Manifest](target: A, name: String): B = target.getClass.getMethod(name).invoke(target).asInstanceOf[B]
}


// --- List support

trait ListOpsExpOpt extends ListOpsExp {
  override def list_concat[A : Manifest](xs1: Exp[List[A]], xs2: Exp[List[A]])(implicit pos: SourceContext): Exp[List[A]] = (xs1, xs2) match {
    case (Def(ListNew(xs1)), Def(ListNew(xs2))) => ListNew(xs1 ++ xs2)
    case _ => super.list_concat(xs1, xs2)
  }
}

trait ListOpsInScala extends ListOps with JSInScala {
  override def list_new[A : Manifest](xs: Seq[A])(implicit pos: SourceContext) = xs.toList
  override def list_fromseq[A : Manifest](xs: Seq[A])(implicit pos: SourceContext) = xs.toList
  override def list_flatMap[A : Manifest, B : Manifest](f: A => List[B])(xs: List[A])(implicit pos: SourceContext): List[B] = xs flatMap f
  override def list_map[A : Manifest, B : Manifest](xs: List[A], f: A => B)(implicit pos: SourceContext): List[B] = xs map f
  override def list_sortby[A : Manifest, B : Manifest : Ordering](l: List[A], f: A => B)(implicit pos: SourceContext) = l.sortBy(f)
  override def list_prepend[A : Manifest](l: List[A], a: A)(implicit pos: SourceContext) = a :: l
  override def list_toarray[A : Manifest](l: List[A])(implicit pos: SourceContext) = l.toArray
  override def list_toseq[A : Manifest](l: List[A])(implicit pos: SourceContext) = l.toSeq
  override def list_concat[A : Manifest](xs1: List[A], xs2: List[A])(implicit pos: SourceContext): List[A] = xs1 ++ xs2
  override def list_cons[A : Manifest](x: A, xs: List[A])(implicit pos: SourceContext) = x :: xs
  override def list_head[A : Manifest](xs: List[A])(implicit pos: SourceContext) = xs.head
  override def list_tail[A : Manifest](xs: List[A])(implicit pos: SourceContext) = xs.tail
  override def list_isEmpty[A : Manifest](xs: List[A])(implicit pos: SourceContext) = xs.isEmpty
  override def list_mkString[A : Manifest](xs: List[A])(implicit pos: SourceContext): String = xs.mkString
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
