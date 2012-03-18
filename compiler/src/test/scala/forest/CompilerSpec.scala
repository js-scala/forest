package forest

import org.specs2.mutable.Specification
import java.io.File
import scalax.file.Path
import forest.backends.Lms

class CompilerSpec extends Specification {
  
  val compiler = new Compiler
  val resourcesDir = Path(new File("compiler/src/test/resources"))
  val targetDir = Path(new File("compiler/target/test/generated"))
  
  targetDir.deleteRecursively(true, true)
  
  "a compiler" should {
    
    "compile a forest file to a Scala virtualizable function" >> {
      compiler.compile(resourcesDir / "article.forest", Nil, new Lms, targetDir)
      (targetDir / "article.scala").exists must beTrue
    }

    "compile a forest file to a Scala virtualizabl function" >> {
      compiler.compile(resourcesDir / "variables.forest", Nil, new Lms, targetDir)
      (targetDir / "variables.scala").exists must beTrue
    }

  }
}