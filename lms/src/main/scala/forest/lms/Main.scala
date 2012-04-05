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
  def apply(foo: Rep[String]): Rep[Tree] = {
    val xs = for (i <- 1 to 5) yield tag("span", Nil, Map("class" -> List(unit(i))), None)
    val div = tag("div", List(tag("span", Nil, Map.empty, Some("a"))) ++ xs, Map("class" -> List(unit("yop "), foo)), Some("root"))
    tree(div)
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