package forest

import scala.virtualization.lms.common.{Base, StringOps}
import scala.reflect.SourceContext

/**
 * Allows to write things like `rep"foo $bar baz"` where `bar` is a Rep[_] and will be quoted.
 */
trait StringInterpolation extends Base with StringOps {
  import language.implicitConversions

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
