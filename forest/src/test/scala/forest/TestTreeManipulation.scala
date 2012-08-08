package forest

import org.scalatest._
import forest._
import virtualization.lms.common._

class TestTreeManipulation extends FileDiffSuite("test-out/") with Suite {

  trait Prog extends ForestPkg with TreeManipulation with LiftAll {
    def all(xs: Rep[List[String]]) = {
      tag2("div")(xs map one)
    }
    def one(x: Rep[String]) = {
      tag("span")(text(x))
    }
    def update(r: Rep[NodeRef]) = {
      r transform ({ (n: Rep[TransformableNode]) =>
        n.find("div").append(one("foo"))
      })
    }
  }

  def testAppending() = testWithOutFile("tree-manipulation") { out =>
    val prog = new Prog with ForestPkgExp with TreeManipulationExp { self =>
      val scalaCodegen = new ScalaGenForestPkg with ScalaGenTreeManipulation { val IR: self.type = self }
      val jsCodegen = new JSGenForestPkg with JSGenTreeManipulation { val IR: self.type = self }
      
      scalaCodegen.emitSource(self.update, "Update", out)
      jsCodegen.emitSource(self.update, "update", out)
    }
  }

}