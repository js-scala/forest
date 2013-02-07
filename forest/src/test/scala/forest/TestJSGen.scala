package forest

import scala.virtualization.lms.common._
import scala.js._
import org.scalatest.Suite

class TestJSGen extends FileDiffSuite("test-out/") with Suite {

  trait Prog { this: JsScala with Forest with LiftJsScala =>
    /**
     * {content: String}
     * div class=message
     *   | Content: {content}
     *   button /btn
     *     | Delete
     */
    def message(content: Rep[String]) = {
      val deleteBtn = el('button)("Delete")
      val msg = el('div, 'class->'message) (
          "Content: " + content,
          deleteBtn
      )
      new Record { val root = msg; val btn = deleteBtn }
    }

    def oneChild(s: Rep[String]) = {
      el('div)(
        txt(s))
    }

    def dynamicChildren(xs: Rep[List[String]]) = {
      val items = for (x <- xs) yield {
        el('li)(x)
      }
      el('ul)(items)
    }

  }

  def testJsGen = testWithOutFile("tree-js") { out =>
    val prog = new Prog with JsScalaExp with ForestExp with LiftJsScala { self =>
      val codegen = new JSGenJsScala with JSGenForest { val IR: self.type = self }
      codegen.emitSource(self.message, "Tree", out)
      codegen.emitSource(self.oneChild, "OneChild", out)
      codegen.emitSource(self.dynamicChildren, "DynamicChildren", out)
    }
  }

}