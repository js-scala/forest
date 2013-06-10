package forest

import org.scalatest.Suite
import scala.js.language.JsScala
import scala.js.exp.JsScalaExp
import scala.js.gen.scala.GenJsScala
import scala.virtualization.lms.common.CompileScala

class TestScalaGen extends FileDiffSuite("test-out/") with Suite {

  trait Prog extends JsScala with Forest {

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
    val prog = new Prog with JsScalaExp with ForestExp with CompileScala { self =>

      override val codegen = new GenJsScala with ScalaGenForest { val IR: self.type = self }

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