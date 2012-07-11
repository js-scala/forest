package forest

import forest.lms._
import scala.virtualization.lms.common.CompileScala

trait Message extends ForestPkg {
  def message(content: Rep[String]) = {
    tree(tag("div", List(text("Content: ", content)), Map("class" -> scala.List("message")), None))
  }
}

class TestScalaGen extends FileDiffSuite {

  override val prefix = "test-out/"

  def testStringGen = {
    withOutFile("tree") {
      val prog = new Message with ForestPkgExp with CompileScala { self =>
        override val codegen = new ScalaGenForestPkg { val IR: self.type = self }
        codegen.emitSource(self.message, "Tree", new java.io.PrintWriter(System.out))
        
        println(compile(self.message).asInstanceOf[String => String]("Bonjour"))
      }
    }
    assertFileEqualsCheck("tree")
  }

  def testXmlGen = {
    withOutFile("tree-xml") {
      val prog = new Message with ForestPkgExp with CompileScala { self =>
        override val codegen = new ScalaGenForestXmlPkg { val IR: self.type = self }
        codegen.emitSource(self.message, "Tree", new java.io.PrintWriter(System.out))
        
        println(compile(self.message).asInstanceOf[String => xml.Node]("Bonjour"))
      }
    }
    assertFileEqualsCheck("tree-xml")
  }

}