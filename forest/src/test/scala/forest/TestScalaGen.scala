package forest

import forest._
import scala.virtualization.lms.common.CompileScala
import org.scalatest.Suite
import java.io.PrintWriter

class TestScalaGen extends FileDiffSuite("test-out/") with Suite {

  trait Message extends ForestPkg {

    def oneChild(content: Rep[String]): Rep[Tree] = {
      tree(tag("div", "class"->scala.List("message"))(List(text("Content: ", content))))
    }

    def severalChildren(s: Rep[String]): Rep[Tree] = {
      val greeted = tag("strong")(List(text(s)))
      tree(tag("div")(List(
        text("Hello "),
        greeted,
        text("!")
      )), "greeted"->greeted)
    }

    def dynamicChildren(xs: Rep[List[String]]): Rep[Tree] = {
      val items = for (x <- xs) yield {
        tag("li")(List(text(x)))
      }
      tree(tag("ul")(items))
    }

  }

  def testXmlGen = testWithOutFile("tree-scala") {
    val prog = new Message with ForestPkgExp with CompileScala { self =>

      override val codegen = new ScalaGenForestPkg { val IR: self.type = self }

      codegen.emitSource(self.oneChild, "Tree", new PrintWriter(System.out))
      val messageCompiled = compile(self.oneChild)
      println(messageCompiled("Bonjour")("root"))

      codegen.emitSource(self.severalChildren, "SeveralChildren", new PrintWriter(System.out))
      val severalChildrenCompiled = compile(self.severalChildren)
      println(severalChildrenCompiled("World")("root"))

      codegen.emitSource(self.dynamicChildren, "DynamicChildren", new PrintWriter(System.out))
      val dynamicChildrenCompiled = compile(self.dynamicChildren)
      println(dynamicChildrenCompiled(scala.List("foo", "bar", "baz"))("root"))

    }
  }

}