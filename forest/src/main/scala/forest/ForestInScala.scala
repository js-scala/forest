package forest

import scala.js._
import scala.xml._

/**
 * Simple interpreter for Forest programs
 */
trait ForestInScala extends Forest with JSInScala {

  override type Node = scala.xml.Node
  override type Tree = Map[String, Node]

  override def nodeManifest = manifest[Node]
  override def treeManifest = manifest[Tree]

  override def forest_tag(name: String, attrs: Map[String, List[Any]], children: List[Node]) = {
    val metadatas = attrs.foldLeft[MetaData](Null) {
      case (metadatas, (name, value)) => new UnprefixedAttribute(name, value.mkString, metadatas)
    }
    Elem(null, name, metadatas, TopScope, children: _*)
  }

  override def forest_text(xs: List[Any]) = Text(xs.mkString)

  override def forest_tree(root: Node, refs: Map[String, Node]) = Map((refs + ("root"->root)).toSeq: _*)

  override def infix_root(tree: Map[String, Node]) = tree("root")

  override def infix_ref(tree: Map[String, Node], ref: String) = tree(ref)
}

trait ForestInScalaPkg extends ForestInScala with ListOpsInScala with ModulesInScala with JSProxyInScala