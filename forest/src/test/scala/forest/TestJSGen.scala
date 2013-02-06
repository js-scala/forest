package forest

import forest._
import scala.virtualization.lms.common._
import scala.js._
import org.scalatest.Suite

class TestJSGen extends FileDiffSuite("test-out/") with Suite {

  trait Message extends ForestPkg with StringOps with Structs with LiftAll {
    /**
     * {content: String}
     * div class=message
     *   | Content: {content}
     *   button /btn
     *     | Delete
     */
    def message(content: Rep[String]) = {
      val deleteBtn = el('button)(
                txt("Delete"))
      val msg = el('div, 'class->'message) (
          txt("Content: " + content),
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
        el('li)(txt(x))
      }
      el2('ul)(items)
    }

  }

  def testJsGen = testWithOutFile("tree-js") { out =>
    val prog = new Message with ForestPkgExp with StringOpsExp with StructExp { self =>
      val codegen = new JSGenForestPkg with JSGenStringOps with JSGenStruct { val IR: self.type = self }
      codegen.emitSource(self.message, "Tree", out)
      codegen.emitSource(self.oneChild, "OneChild", out)
      codegen.emitSource(self.dynamicChildren, "DynamicChildren", out)
    }
  }

}