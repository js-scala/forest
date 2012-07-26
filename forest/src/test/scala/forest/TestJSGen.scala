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
      val btn = tag("button")(List(text("Delete")))
      tree(tag("div", "class"->scala.List("message"))(List(
        text("Content: ", content),
        btn
      )), "btn"->btn)
    }

    def oneChild(s: Rep[String]): Rep[Tree] = {
      tree(tag("div")(List(text(s))))
    }

    def dynamicChildren(xs: Rep[List[String]]): Rep[Tree] = {
      val items = for (x <- xs) yield {
        tag("li")(List(text(x)))
      }
      tree(tag("ul")(items))
    }

  }

  def testJsGen = testWithOutFile("tree-js") {
    val prog = new Message with ForestPkgExp { self =>
      val codegen = new JSGenForestPkg { val IR: self.type = self }
      codegen.emitSource(self.message, "Tree", new java.io.PrintWriter(System.out))
      codegen.emitSource(self.oneChild, "OneChild", new java.io.PrintWriter(System.out))
      codegen.emitSource(self.dynamicChildren, "DynamicChildren", new java.io.PrintWriter(System.out))
    }
  }

}