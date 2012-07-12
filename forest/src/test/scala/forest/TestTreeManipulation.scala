package forest

import org.scalatest._
import forest.lms._

class TestTreeManipulation extends FileDiffSuite("test-out/") with Suite {

  trait Prog extends ForestPkg with TreeManipulation {
    def all(xs: Rep[List[String]]): Rep[Tree] = {
      tree(tag("div", xs map one, Map.empty, None))
    }
    def one(x: Rep[String]): Rep[Tree] = {
      tree(tag("span", List(text(x)), Map.empty, None))
    }
    def update(p: Rep[(NodeRef, String)]) {
      val r = tuple2_get1(p)
      val x = tuple2_get2(p)
      r transform ({ (n: Rep[TransformableNode]) =>
        n.find("div").append(one(x))
      })
    }
    def update2(r: Rep[NodeRef]) {
      r transform ({ (n: Rep[TransformableNode]) =>
        n.find("div").append(one("foo"))
      })
    }
  }

  def testAppending() = testWithOutFile("tree-manipulation") {
    val prog = new Prog with ForestPkgExp with TreeManipulationExp { self =>
      val scalaCodegen = new ScalaGenForestXmlPkg with ScalaGenTreeManipulation { val IR: self.type = self }
      val jsCodegen = new JSGenForestPkg with JSGenTreeManipulation { val IR: self.type = self }
      
      scalaCodegen.emitSource(self.update2, "Update", new java.io.PrintWriter(System.out))
      jsCodegen.emitSource(self.update2, "update", new java.io.PrintWriter(System.out))
    }
  }

}