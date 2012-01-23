package forest.backends

import forest.ast.Document
import scalax.file.Path

trait Backend {
  /**
   * Generate code from a document in a target directory
   * @param namespace List of resulting object qualified name (e.g. List("myApp", "users", "profile")). Must contain at least one element.
   * @param document Source document
   * @param targetDirectory Target base directory
   */
  def generate(document: Document, namespace: List[String], targetDirectory: Path)
}