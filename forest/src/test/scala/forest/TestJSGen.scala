package forest

import scala.js.language.JsScala
import scala.js.exp.JsScalaExp
import scala.js.gen.js.GenJsScala
import org.scalatest.Suite

import scala.virtualization.lms.common.Record

class TestJSGen extends FileDiffSuite("test-out/") with Suite {

  trait Prog extends JsScala with Forest {
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
    val prog = new Prog with JsScalaExp with ForestExp { self =>
      val codegen = new GenJsScala with JSGenForest { val IR: self.type = self }
      codegen.emitSource(self.message, "Tree", out)
      codegen.emitSource(self.oneChild, "OneChild", out)
      codegen.emitSource(self.dynamicChildren, "DynamicChildren", out)
    }
  }

}