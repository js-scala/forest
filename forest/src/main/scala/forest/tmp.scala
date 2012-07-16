package forest

import scala.js.JSGenBase
import scala.js.JSGenEffect
import scala.js.JSInScala
import scala.js.JSProxyExp
import scala.virtualization.lms.common.Base
import scala.virtualization.lms.common.BaseExp
import scala.virtualization.lms.common.EffectExp
import scala.virtualization.lms.common.ScalaGenBase
import scala.virtualization.lms.common.ScalaGenEffect

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
  case class Field[A, B : Manifest](target: Exp[A], name: String) extends Def[B]
  override def field[A, B : Manifest](target: Rep[A], name: String): Rep[B] = toAtom(Field(target, name)) // FIXME Why do I need to write toAtom explicitly?
}

trait JSGenFields extends JSGenBase {
  val IR: FieldsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Field(o, n) => emitValDef(sym, quote(o) + "." + n)
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenFields extends ScalaGenBase {
  val IR: FieldsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Field(o, n) => emitValDef(sym, quote(o) + "." + n)
    case _ => super.emitNode(sym, rhs)
  }
}

trait FieldsInScala extends Fields with JSInScala {
  def field[A, B : Manifest](target: A, name: String): B = target.getClass.getMethod(name).invoke(target).asInstanceOf[B]
}


// --- Rep[List] support

trait ListOps2 extends Base {

  object List {
    def apply[A : Manifest](xs: Rep[A]*) = list_new(xs)
  }

  implicit def repToOps[A : Manifest](xs: Rep[List[A]]) = new ListOps2Cls(xs)

  class ListOps2Cls[A : Manifest](xs: Rep[List[A]]) {
    def flatMap[B : Manifest](f: Rep[A] => Rep[List[B]]) = list_flatMap(f)(xs)
    def map[B : Manifest](f: Rep[A] => Rep[B]) = list_map(f)(xs)
    def ++ (xs2: Rep[List[A]]) = list_concat(xs, xs2)
    def mkString = list_mkString(xs)
  }

  def list_new[A : Manifest](xs: Seq[Rep[A]]): Rep[List[A]]
  def list_flatMap[A : Manifest, B : Manifest](f: Rep[A] => Rep[List[B]])(xs: Rep[List[A]]): Rep[List[B]]
  def list_map[A : Manifest, B : Manifest](f: Rep[A] => Rep[B])(xs: Rep[List[A]]): Rep[List[B]]
  def list_concat[A : Manifest](xs1: Rep[List[A]], xs2: Rep[List[A]]): Rep[List[A]]
  def list_mkString[A : Manifest](xs: Rep[List[A]]): Rep[String]
}

trait ListOps2Exp extends ListOps2 with EffectExp {

  def list_new[A : Manifest](xs: Seq[Rep[A]]) = ConstList(xs)
  def list_flatMap[A : Manifest, B : Manifest](f: Exp[A] => Exp[List[B]])(xs: Exp[List[A]]) = {
    val x = fresh[A]
    val b = reifyEffects(f(x))
    reflectEffect(ListFlatMap(xs, x, b), Alloc() andAlso summarizeEffects(b).star)
  }
  def list_map[A : Manifest, B : Manifest](f: Exp[A] => Exp[B])(xs: Exp[List[A]]) = {
    val x = fresh[A]
    val b = reifyEffects(f(x))
    reflectEffect(ListMap(xs, x, b), Alloc() andAlso summarizeEffects(b).star)
  }
  def list_concat[A : Manifest](xs1: Exp[List[A]], xs2: Exp[List[A]]) = ListConcat(xs1, xs2)
  def list_mkString[A : Manifest](xs: Exp[List[A]]) = ListMkString(xs)

  case class ConstList[A : Manifest](xs: Seq[Rep[A]]) extends Def[List[A]]
  case class ListFlatMap[A, B : Manifest](l: Exp[List[A]], x: Sym[A], block: Block[List[B]]) extends Def[List[B]]
  case class ListMap[A, B : Manifest](l: Exp[List[A]], x: Sym[A], block: Block[B]) extends Def[List[B]]
  case class ListConcat[A : Manifest](l1: Exp[List[A]], l2: Exp[List[A]]) extends Def[List[A]]
  case class ListMkString[A : Manifest](l: Exp[List[A]]) extends Def[String]

  override def syms(e: Any): List[Sym[Any]] = e match {
    case ConstList(xs) => xs.flatMap(syms).toList
    case ListFlatMap(l, _, b) => syms(l) ::: syms(b)
    case ListMap(l, _, b) => syms(l) ::: syms(b)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ConstList(xs) => xs.flatMap(effectSyms).toList
    case ListFlatMap(_, x, b) => x :: effectSyms(b)
    case ListMap(_, x, b) => x :: effectSyms(b)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ConstList(xs) => xs.flatMap(freqNormal).toList
    case ListFlatMap(l, _, b) => freqNormal(l) ::: freqHot(b)
    case ListMap(l, _, b) => freqNormal(l) ::: freqHot(b)
    case _ => super.symsFreq(e)
  }

}

trait ListOps2Opt extends ListOps2Exp {
  override def list_concat[A : Manifest](xs1: Exp[List[A]], xs2: Exp[List[A]]): Exp[List[A]] = (xs1, xs2) match {
    case (Def(ConstList(xs1)), Def(ConstList(xs2))) => ConstList(xs1 ++ xs2)
    case _ => super.list_concat(xs1, xs2)
  }
}

trait ScalaGenListOps2 extends ScalaGenEffect {
  val IR: ListOps2Exp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ConstList(xs) => {
      emitValDef(sym, "List(" + xs.map(quote).mkString(", ") + ")")
    }
    case ListFlatMap(l, x, b) => {
      stream.println("val " + quote(sym) + " = " + quote(l) + ".flatMap { " + quote(x) + " => ")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
    }
    case ListMap(l, x, b) => {
      stream.println("val " + quote(sym) + " = " + quote(l) + ".map { " + quote(x) + " =>")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")
    }
    case ListConcat(l1, l2) => emitValDef(sym, quote(l1) + " ++ " + quote(l2))
    case _ => super.emitNode(sym, rhs)
  }
}

trait JSGenListOps2 extends JSGenEffect {
  val IR: ListOps2Exp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ConstList(values) => {
      emitValDef(sym, values.map(quote).mkString("[", ",", "]") + ";")
    }
    // TODO reuse JSArrays
    case ListFlatMap(l, x, b) => {
      stream.println("var " + quote(sym) + " = [];")
      val i = fresh[Int]
      stream.println("for(var " + quote(i) + " = 0 ; " + quote(i) + " < " + quote(l) + ".length ; " + quote(i) + "++){")
      stream.println(quote(sym) + ".splice.apply(" + quote(sym) + ", [" + quote(sym) + ".length, 0].concat((function(" + quote(x) + "){")
      emitBlock(b)
      stream.println("return " + quote(getBlockResult(b)))
      stream.println("})(" + quote(l) + "[" + quote(i) + "])));")
      stream.println("}")
    }
    case ListMap(l, x, b) => {
      stream.print("var " + quote(sym) + "=" + quote(l) + ".map(")
      stream.println("function(" + quote(x) + "){")
      emitBlock(b)
      stream.println("return " + quote(getBlockResult(b)))
      stream.println("});")
    }
    case ListConcat(l1, l2) => emitValDef(sym, quote(l1) + ".concat(" + quote(l2) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ListOps2InScala extends ListOps2 with JSInScala {

  def list_new[A : Manifest](xs: Seq[A]): List[A] = xs.toList

  def list_flatMap[A : Manifest, B : Manifest](f: A => List[B])(xs: List[A]): List[B] = xs flatMap f

  def list_map[A : Manifest, B : Manifest](f: A => B)(xs: List[A]): List[B] = xs map f

  def list_concat[A : Manifest](xs1: List[A], xs2: List[A]): List[A] = xs1 ++ xs2

  def list_mkString[A : Manifest](xs: List[A]): String = xs.mkString

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
