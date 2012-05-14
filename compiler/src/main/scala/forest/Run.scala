package forest
import forest.backends._
import scalax.file.Path

object Run extends App {
  
  if (args.size < 2) {
    sys.error("Usage: run <source> <target>")
  }
  
  val compiler = new Compiler
  val sourceDir = Path(new java.io.File(args(0)))
  val targetDir = Path(new java.io.File(args(1)))
  
  compiler.compile(sourceDir, targetDir)
}