package forest

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.reflect.SourceContext

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

import scala.js._

// Partial support of ScalaOpsPkg
trait JSCodeGenPkg extends JSGenNumericOps with JSGenOrderingOps with JSGenStringOps
    with JSGenBooleanOps with JSGenVariables with JSGenFunctions with JSGenEqual
    with JSGenIfThenElse with JSGenWhile with JSGenTupleOps with JSGenListOps
    with JSGenObjectOps { val IR: ScalaOpsPkgExp with TupledFunctionsExp }