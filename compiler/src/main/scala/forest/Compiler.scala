package forest

import forest.backend.Lms
import scalax.file.Path

class Compiler {

  private val parser = new Parser

  /**
   * Compile all .forest files found (recursively) in `sourceDir` into `outputDir`
   */
  def compile(sourceDir: Path, outputDir: Path) {
    for (source <- sourceDir ** "*.forest") {
      val segments = source.relativize(sourceDir).segments
      compile(source, segments.take(segments.size - 1).toList, outputDir)
    }
  }

  def compile(source: Path, namespace: List[String], outputDir: Path) {
    val result = parser.parseAll(parser.document, source.slurpString)
    result.map { document =>
      Lms.generate(document, namespace :+ source.simpleName, outputDir)
    }.getOrElse(println("Huston, we have a problem."))
  }

}