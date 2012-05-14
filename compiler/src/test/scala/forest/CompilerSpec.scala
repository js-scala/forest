package forest

import org.specs2.mutable.Specification
import java.io.File
import scalax.file.Path

class CompilerSpec extends Specification {
  
  val compiler = new Compiler
  val resourcesDir = Path(new File("src/resources"))
  val targetDir = Path(new File("target/test/generated"))
  
  targetDir.deleteRecursively(true, true)
  
  "a compiler" should {
    
    "compile a forest file to a Scala virtualizable function" >> {
      compiler.compile(resourcesDir / "article.forest", Nil, targetDir)
      (targetDir / "article.scala").exists must beTrue
    }

    "compile a forest file to a Scala virtualizable function" >> {
      compiler.compile(resourcesDir / "variables.forest", Nil, targetDir)
      (targetDir / "variables.scala").exists must beTrue
    }

  }
}