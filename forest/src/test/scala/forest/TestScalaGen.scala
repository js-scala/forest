package forest

import forest.lms._
import scala.virtualization.lms.common.CompileScala
import org.scalatest.Suite
import java.io.PrintWriter

class TestScalaGen extends FileDiffSuite("test-out/") with Suite {

  trait Message extends ForestPkg {

    def oneChild(content: Rep[String]): Rep[Tree] = {
      tree(tag("div", "class"->scala.List("message"))(List(text("Content: ", content))))
    }

    def severalChildren(s: Rep[String]): Rep[Tree] = {
      tree(tag("div")(List(
        text("Hello "),
        tag("strong")(List(text(s))),
        text("!")
      )))
    }

    def dynamicChildren(xs: Rep[List[String]]): Rep[Tree] = {
      val items = for (x <- xs) yield {
        tag("li")(List(text(x)))
      }
      tree(tag("ul")(items))
    }

  }

  def testStringGen = testWithOutFile("tree") {
    val prog = new Message with ForestStringPkgExp with CompileScala { self =>

      override val codegen = new ScalaGenForestPkg { val IR: self.type = self }

      codegen.emitSource(self.oneChild, "Tree", new PrintWriter(System.out))
      val messageCompiled = compile(self.oneChild)
      println(messageCompiled("Bonjour"))

      codegen.emitSource(self.severalChildren, "SeveralChildren", new PrintWriter(System.out))
      val severalChildrenCompiled = compile(self.severalChildren)
      println(severalChildrenCompiled("World"))

      codegen.emitSource(self.dynamicChildren, "DynamicChildren", new PrintWriter(System.out))
      val dynamicChildrenCompiled = compile(self.dynamicChildren)
      println(dynamicChildrenCompiled(scala.List("foo", "bar", "baz")))

    }
  }

  def testXmlGen = testWithOutFile("tree-xml") {
    val prog = new Message with ForestXmlPkgExp with CompileScala { self =>

      override val codegen = new ScalaGenForestXmlPkg { val IR: self.type = self }

      codegen.emitSource(self.oneChild, "Tree", new PrintWriter(System.out))
      val messageCompiled = compile(self.oneChild)
      println(messageCompiled("Bonjour"))

      codegen.emitSource(self.severalChildren, "SeveralChildren", new PrintWriter(System.out))
      val severalChildrenCompiled = compile(self.severalChildren)
      println(severalChildrenCompiled("World"))

      codegen.emitSource(self.dynamicChildren, "DynamicChildren", new PrintWriter(System.out))
      val dynamicChildrenCompiled = compile(self.dynamicChildren)
      println(dynamicChildrenCompiled(scala.List("foo", "bar", "baz")))

    }
  }

}