package forest

import forest._
import scala.virtualization.lms.common._
import org.scalatest.Suite
import java.io.PrintWriter

class TestScalaGen extends FileDiffSuite("test-out/") with Suite {

  trait Message extends ForestPkg with LiftAll {

    def oneChild(content: Rep[String]) = {
      tag("div", "class"->scala.List("message"))(
        text("Content: ", content))
    }

    def severalChildren(s: Rep[String]) = {
      val g = tag("strong")(text(s))
      new Record {
        val root = tag("div")(
          text("Hello "),
          g,
          text("!")
        )
        val greeted = g
      }
    }

    def dynamicChildren(xs: Rep[List[String]]) = {
      val items = for (x <- xs) yield {
        tag("li")(text(x))
      }
      tag2("ul")(items)
    }

  }

  def testXmlGen = testWithOutFile("tree-scala") { out =>
    val prog = new Message with ForestPkgExp with CompileScala { self =>

      override val codegen = new ScalaGenForestPkg { val IR: self.type = self }

      codegen.emitSource(self.oneChild, "Tree", out)
      // val messageCompiled = compile(self.oneChild)
      // println(messageCompiled("Bonjour"))

      codegen.emitSource(self.severalChildren, "SeveralChildren", out)
      // val severalChildrenCompiled = compile(self.severalChildren)
      // println(severalChildrenCompiled("World"))

      codegen.emitSource(self.dynamicChildren, "DynamicChildren", out)
      // val dynamicChildrenCompiled = compile(self.dynamicChildren)
      // println(dynamicChildrenCompiled(scala.List("foo", "bar", "baz")))

      codegen.emitDataStructures(out)
    }
  }

}