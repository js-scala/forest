package forest

import scala.js._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
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


trait QuoteGen { this: GenericCodegen =>

  trait Quote {
    def q(args: Any*): String
  }

  implicit def stringContextToQuote(ctx: StringContext): Quote = new Quote {
    def q(args: Any*) = {
      ctx.checkLengths(args)
      val pi = ctx.parts.iterator
      val ai = args.iterator
      val bldr = new java.lang.StringBuilder(StringContext.treatEscapes(pi.next()))
      while (ai.hasNext) {
        bldr.append(ai.next match {
          case e: IR.Exp[_] => quote(e)
          case a => a
        })
        bldr.append(StringContext.treatEscapes(pi.next()))
      }
      bldr.toString
    }
  }
}

/**
 * Allows to write things like `rep"foo $bar baz"` where `bar` is a Rep[_] and will be quoted.
 */
trait StringInterpolation extends Base { this: StringOps =>

  trait RepInterpolator {
    def rep(args: Any*): Rep[String]
  }
  implicit def stringContextToQ(ctx: StringContext): RepInterpolator = new RepInterpolator {
    def rep(args: Any*) = {
      ctx.checkLengths(args)
      string_interpolation(ctx.parts, args)
    }
  }

  // TODO reify in a case class StringInterpolation to generate more efficient code (using buffers)
  def string_interpolation(parts: Seq[String], args: Seq[Any]): Rep[String] = {
    import StringContext.{treatEscapes => esc}
    (args zip parts.tail).foldLeft(unit(esc(parts.head))) { case (acc, (arg, part)) =>
      arg match {
        case r: Rep[_] => acc + r + esc(part)
        case _ => acc + arg.toString + esc(part)
      }
    }
  }
}
