package forest

import org.specs2.mutable.Specification
import java.io.File
import scalax.file.Path
import compiler.Compiler

class CompilerSpec extends Specification {
  
  val compiler = new Compiler
  val resourcesDir = Path(new File("src/resources"))
  val targetDir = Path(new File("target/test/generated"))
  
  targetDir.deleteRecursively(true, true)
  
  "a compiler" should {
    
    "compile a forest file to a Scala virtualizable function" >> {
      compiler.compile(resourcesDir, targetDir)
      (targetDir / "resources.scala").exists must beTrue
    }

  }
}