package forest.lms

import virtualization.lms.common._
import virtualization.lms.internal._
import js._

/**
 * {{{
 *   {foo: String}
 *     div class="yop {foo}"
 * }}}
 */
trait TemplateFoo { this: Forest =>
  def apply(foo: Rep[String]): Rep[Node] = {
    tag("div", Map("class" -> List("yop ", foo)))
  }
}

object Main extends App {

  new TemplateFoo with ForestExp { self =>
    val codegen = new ForestJSCodegen { val IR: self.type = self }
    codegen.emitSource(apply _, "tmpl", new java.io.PrintWriter(System.out))
  }

  new TemplateFoo with ForestExp { self =>
    val codegen = new ForestScalaCodegen { val IR: self.type = self }
    codegen.emitSource(apply _, "Tmpl", new java.io.PrintWriter(System.out))
  }
}