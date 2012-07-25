package forest

import org.scalatest._
import forest.lms._

class TestTreeManipulation extends FileDiffSuite("test-out/") with Suite {

  trait Prog extends ForestPkg with TreeManipulation {
    def all(xs: Rep[List[String]]): Rep[Tree] = {
      tree(tag("div")(xs map one))
    }
    def one(x: Rep[String]): Rep[Tree] = {
      tree(tag("span")(List(text(x))))
    }
    def update(r: Rep[NodeRef]) {
      r transform ({ (n: Rep[TransformableNode]) =>
        n.find("div").append(one("foo"))
      })
    }
  }

  def testAppending() = testWithOutFile("tree-manipulation") {
    val prog = new Prog with ForestXmlPkgExp with TreeManipulationExp { self =>
      val scalaCodegen = new ScalaGenForestXmlPkg with ScalaGenTreeManipulation { val IR: self.type = self }
      val jsCodegen = new JSGenForestPkg with JSGenTreeManipulation { val IR: self.type = self }
      
      scalaCodegen.emitSource(self.update, "Update", new java.io.PrintWriter(System.out))
      jsCodegen.emitSource(self.update, "update", new java.io.PrintWriter(System.out))
    }
  }

}