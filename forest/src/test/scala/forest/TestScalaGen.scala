package forest

import scala.virtualization.lms.common._
import org.scalatest.Suite
import js._

class TestScalaGen extends FileDiffSuite("test-out/") with Suite {

  trait Prog { this: JsScala with Forest with LiftJsScala =>

    def oneChild(content: Rep[String]) = {
      el('div, 'class->'message, "data-id"->42)(
        "Content: " + content)
    }

    def severalChildren(s: Rep[String]) = {
      val g = el('strong)(s)
      new Record {
        val root = el('div)(
          "Hello ", g, "!"
        )
        val greeted = g
      }
    }

    def dynamicChildren(xs: Rep[List[String]]) = {
      val items = for (x <- xs) yield {
        el('li)(x)
      }
      el('ul)(items)
    }

  }

  def testXmlGen = testWithOutFile("tree-scala") { out =>
    val prog = new Prog with JsScalaExp with ForestExp with LiftJsScala with CompileScala { self =>

      override val codegen = new ScalaGenJsScala with ScalaGenForest { val IR: self.type = self }

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