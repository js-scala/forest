package forest

import forest.lms._
import scala.virtualization.lms.common.CompileScala
import org.scalatest.Suite

class TestJSGen extends FileDiffSuite("test-out/") with Suite {

  trait Message extends ForestPkg {
    /**
     * {content: String}
     * div class=message
     *   | Content: {content}
     *   button /btn
     *     | Delete
     */
    def message(content: Rep[String]): Rep[Tree] = {
      tree(tag("div", List(
          text("Content: ", content),
          tag("button", List(text("Delete")), Map.empty, Some("btn"))
      ), Map("class" -> scala.List("message")), None))
    }

    def oneChild(s: Rep[String]): Rep[Tree] = {
      tree(tag("div", List(text(s)), Map.empty, None))
    }

    def dynamicChildren(xs: Rep[List[String]]): Rep[Tree] = {
      val items = for (x <- xs) yield {
        tag("li", List(text(x)), Map.empty, None)
      }
      tree(tag("ul", items, Map.empty, None))
    }

  }

  def testJsGen = testWithOutFile("tree-js") {
    val prog = new Message with ForestStringPkgExp { self =>
      val codegen = new JSGenForestPkg { val IR: self.type = self }
      codegen.emitSource(self.message, "Tree", new java.io.PrintWriter(System.out))
      codegen.emitSource(self.oneChild, "OneChild", new java.io.PrintWriter(System.out))
      codegen.emitSource(self.dynamicChildren, "DynamicChildren", new java.io.PrintWriter(System.out))
    }
  }

}