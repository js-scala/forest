package forest

import org.scalatest._
import virtualization.lms.common._
import js._

class TestTreeManipulation extends FileDiffSuite("test-out/") with Suite {

  trait Prog { this: JsScala with Forest with TreeManipulation with LiftJsScala =>
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
    val prog = new Prog with JsScalaExp with ForestExp with TreeManipulationExp with LiftJsScala { self =>
      val scalaCodegen = new ScalaGenJsScala with ScalaGenForest with ScalaGenTreeManipulation { val IR: self.type = self }
      val jsCodegen = new JSGenJsScala with JSGenForest with JSGenTreeManipulation { val IR: self.type = self }
      
      scalaCodegen.emitSource(self.update, "Update", out)
      jsCodegen.emitSource(self.update, "update", out)
    }
  }

}