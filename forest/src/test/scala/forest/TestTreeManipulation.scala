package forest

import org.scalatest._
import virtualization.lms.common._
import scala.js.language.JsScala
import scala.js.exp.JsScalaExp

class TestTreeManipulation extends FileDiffSuite("test-out/") with Suite {

  trait Prog extends JsScala with Forest with TreeManipulation {
    def all(xs: Rep[List[String]]) = {
      el('div)(xs map one)
    }
    def one(x: Rep[String]) = {
      el('span)(x)
    }
    def update(r: Rep[NodeRef]) = {
      r transform ({ (n: Rep[TransformableNode]) =>
        n.find("div").append(one("foo"))
      })
    }
  }

  def testAppending() = testWithOutFile("tree-manipulation") { out =>
    val prog = new Prog with JsScalaExp with ForestExp with TreeManipulationExp { self =>
      val scalaCodegen = new scala.js.gen.scala.GenJsScala with ScalaGenForest with ScalaGenTreeManipulation { val IR: self.type = self }
      val jsCodegen = new scala.js.gen.js.GenJsScala with JSGenForest with JSGenTreeManipulation { val IR: self.type = self }
      
      scalaCodegen.emitSource(self.update, "Update", out)
      jsCodegen.emitSource(self.update, "update", out)
    }
  }

}