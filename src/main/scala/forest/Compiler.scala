package forest

import forest.backends.Backend
import scalax.file.Path

class Compiler {
  
  private val parser = new Parser

  def compile(source: Path, backend: Backend, outputDir: Path) {
    val result = parser.parseAll(parser.document, source.slurpString)
    if (result.successful) {
      backend.generate(result.get, List(source.simpleName), outputDir)
    } else {
      println("Huston, we have a problem: " + result)
    }
  }
}