package forest.backends

import forest.ast.Document
import scalax.file.Path

trait Backend {
  def generate(source: Path, document: Document, targetDirectory: Path)
}