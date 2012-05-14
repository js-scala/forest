package forest

import scalax.file.Path

object Run extends App {

  if (args.size < 2) {
    sys.error("Usage: run <source> <target>")
  }

  val sourceDir = Path(new java.io.File(args(0)))
  val targetDir = Path(new java.io.File(args(1)))

  Compiler.compile(sourceDir, targetDir)
}