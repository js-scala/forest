package forest
import forest.backends.ScalaText
import forest.backends.JsDom
import scalax.file.Path

object Run extends App {
  
  if (args.size < 2) {
    sys.error("Missing parameters")
  }
  
  val compiler = new Compiler
  val backend = if (args.size > 2 && args(2) == "js") new JsDom else new ScalaText
  compiler.compile(Path(new java.io.File(args(0))), backend, Path(new java.io.File(args(1))))
}