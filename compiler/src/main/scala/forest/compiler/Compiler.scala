package forest.compiler

import scalax.file.Path

class Compiler {

  private val parser = new Parser

  /**
   * Compile all .forest files found (recursively) in `sourceDir` into `outputDir`
   */
  def compileRecursively(sourceDir: Path, outputDir: Path) {
    /*for (source <- sourceDir ** "*.forest") {
      val segments = source.relativize(sourceDir).segments
      compile(source, segments.drop(1).take(segments.size - 2).toList, outputDir)
    }*/
  }

  /**
   * Compile .forest files found in `sourceDir` into `outputDir`
   */
  def compile(sourceDir: Path, outputDir: Path, imports: Seq[String] = Nil, mixins: Seq[String] = Nil) {
    val templates = for (source <- (sourceDir * "*.forest").toList) yield {
      parser.parse(source.slurpString) match {
        case Right(document) => Right(Lms.generate(document, source.simpleName, outputDir))
        case Left(error) => {
          println("Unable to parse file '%s': %s".format(source.name, error))
          Left(error)
        }
      }
    }
    (outputDir / (sourceDir.simpleName + ".scala")).write(
        """|package ir // TODO remove that
           |
           |import forest._
           |%s
           |
           |trait %s extends ForestPkg %s{
           |  import collection.immutable.{List => SList}
           |  trait %s {
           |    %s
           |  }
           |}""".stripMargin.format(
               (for (`import` <- imports) yield "import %s".format(`import`)).mkString("\n"),
               sourceDir.simpleName,
               (for (mixin <- mixins) yield "with %s ".format(mixin)).mkString,
               sourceDir.simpleName,
               (for (template <- templates; t <- template.right.toSeq) yield t).mkString
           )
    )
  }

}

object Compiler extends Compiler