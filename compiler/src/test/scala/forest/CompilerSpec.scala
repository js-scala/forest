package forest

import org.specs2.mutable.Specification
import java.io.File
import scalax.file.Path
import forest.backends.ScalaText
import forest.backends.JsDom

class CompilerSpec extends Specification {
  
  val compiler = new Compiler
  val resourcesDir = Path(new File("compiler/src/test/resources"))
  val targetDir = Path(new File("compiler/target/test/generated"))
  
  targetDir.deleteRecursively(true, true)
  
  "a compiler" should {
    
    "compile a forest file to a Scala function" >> {
      compiler.compile(resourcesDir / "article.forest", Nil, new ScalaText("forest.Play2Json"), targetDir)
      (targetDir / "article.scala").exists must beTrue
    }

    "compile a forest file to a JavaScript function" >> {
      compiler.compile(resourcesDir / "article.forest", Nil, new JsDom, targetDir)
      (targetDir / "article.js").exists must beTrue
    }

    "compile a forest file to a Scala function" >> {
      compiler.compile(resourcesDir / "variables.forest", Nil, new ScalaText("forest.Play2Json"), targetDir)
      (targetDir / "variables.scala").exists must beTrue
    }

    "compile a forest file to a JavaScript function" >> {
      compiler.compile(resourcesDir / "variables.forest", Nil, new JsDom, targetDir)
      (targetDir / "variables.js").exists must beTrue
    }

  }
}