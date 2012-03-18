package forest
import forest.backends._
import scalax.file.Path

object Run extends App {
  
  if (args.size < 2) {
    sys.error("Missing parameters")
  }
  
  val compiler = new Compiler
  val backend = args(2) match {
    case "js" => new JsDom
    case "scala" => new ScalaText(args(3))
    case "lms" => new Lms
  }
  val sourceDir = Path(new java.io.File(args(0)))
  val targetDir = Path(new java.io.File(args(1)))
  
  for (source <- sourceDir ** "*.forest") {
    val segments = source.relativize(sourceDir).segments
    compiler.compile(source, segments.take(segments.size - 1).toList, backend, targetDir)
  }
}