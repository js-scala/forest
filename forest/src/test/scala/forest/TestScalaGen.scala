package forest

import forest._
import scala.virtualization.lms.common._
import org.scalatest.Suite
import java.io.PrintWriter

class TestScalaGen extends FileDiffSuite("test-out/") with Suite {

  trait Message extends ForestPkg with StringOps with Structs with LiftAll {

    def oneChild(content: Rep[String]) = {
      el('div, 'class->'message, "data-id"->42)(
        txt("Content: " + content))
    }

    def severalChildren(s: Rep[String]) = {
      val g = el('strong)(txt(s))
      new Record {
        val root = el('div)(
          txt("Hello "), g, txt("!")
        )
        val greeted = g
      }
    }

    def dynamicChildren(xs: Rep[List[String]]) = {
      val items = for (x <- xs) yield {
        el('li)(txt(x))
      }
      el2('ul)(items)
    }

  }

  def testXmlGen = testWithOutFile("tree-scala") { out =>
    val prog = new Message with ForestPkgExp with StringOpsExp with StructExp with CompileScala { self =>

      override val codegen = new ScalaGenForestPkg with ScalaGenStringOps with ScalaGenStruct { val IR: self.type = self }

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