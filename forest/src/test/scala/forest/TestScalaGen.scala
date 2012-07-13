package forest

import forest.lms._
import scala.virtualization.lms.common.CompileScala
import org.scalatest.Suite

class TestScalaGen extends FileDiffSuite("test-out/") with Suite {

  trait Message extends ForestPkg {
    def message(content: Rep[String]) = {
      tree(tag("div", List(text("Content: ", content)), Map("class" -> scala.List("message")), None))
    }
  }

  def testStringGen = testWithOutFile("tree") {
    val prog = new Message with ForestStringPkgExp with CompileScala { self =>
      override val codegen = new ScalaGenForestPkg { val IR: self.type = self }
      codegen.emitSource(self.message, "Tree", new java.io.PrintWriter(System.out))

      println(compile(self.message).asInstanceOf[String => String]("Bonjour"))
    }
  }

  def testXmlGen = testWithOutFile("tree-xml") {
    val prog = new Message with ForestXmlPkgExp with CompileScala { self =>
      override val codegen = new ScalaGenForestXmlPkg { val IR: self.type = self }
      codegen.emitSource(self.message, "Tree", new java.io.PrintWriter(System.out))

      println(compile(self.message).asInstanceOf[String => xml.Node]("Bonjour"))
    }
  }

}