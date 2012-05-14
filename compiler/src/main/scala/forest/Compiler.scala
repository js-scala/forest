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
    parser.parse(source.slurpString) match {
      case Right(document) => Lms.generate(document, namespace :+ source.simpleName, outputDir)
      case Left(error) => println("Unable to parse file '%s': %s".format(source.name, error))
    }
  }

}

object Compiler extends Compiler