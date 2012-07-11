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
    def message(content: Rep[String]) = {
      tree(tag("div", List(
          text("Content: ", content),
          tag("button", List(text("Delete")), Map.empty, Some("btn"))
      ), Map("class" -> scala.List("message")), None))
    }
  }

  def testJsGen = testWithOutFile("tree-js") {
    val prog = new Message with ForestPkgExp { self =>
      val codegen = new JSGenForestPkg { val IR: self.type = self }
      codegen.emitSource(self.message, "Tree", new java.io.PrintWriter(System.out))
    }
  }

}