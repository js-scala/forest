package forest

import forest._
import scala.virtualization.lms.common._
import org.scalatest.Suite

class TestJSGen extends FileDiffSuite("test-out/") with Suite {

  trait Message extends ForestPkg with LiftAll {
    /**
     * {content: String}
     * div class=message
     *   | Content: {content}
     *   button /btn
     *     | Delete
     */
    def message(content: Rep[String]) = {
      val b = tag("button")(
                text("Delete"))
      val r = tag("div", "class"->scala.List("message")) (
          text("Content: ", content),
          b
      )
      new Record { val root = r; val btn = b }
    }

    def oneChild(s: Rep[String]) = {
      tag("div")(
        text(s))
    }

    def dynamicChildren(xs: Rep[List[String]]) = {
      val items = for (x <- xs) yield {
        tag("li")(text(x))
      }
      tag2("ul")(items)
    }

  }

  def testJsGen = testWithOutFile("tree-js") { out =>
    val prog = new Message with ForestPkgExp { self =>
      val codegen = new JSGenForestPkg { val IR: self.type = self }
      codegen.emitSource(self.message, "Tree", out)
      codegen.emitSource(self.oneChild, "OneChild", out)
      codegen.emitSource(self.dynamicChildren, "DynamicChildren", out)
    }
  }

}