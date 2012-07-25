package forest

import scala.js.JSInScala
import scala.js.JSProxyInScala

/**
 * Simple interpreter for Forest programs
 */
trait ForestInScala extends Forest with JSInScala {

  override type Node = String
  override type Tree = String

  override def nodeManifest = manifest[Node]
  override def treeManifest = manifest[Tree]

  override def forest_tag(name: String, attrs: Map[String, List[Any]], children: List[String], ref: Option[String]): String = {
    val formattedAttrs = attrs.foldLeft(""){
      case (attrs, (name, value)) => attrs ++ " %s=\"%s\"".format(name, value.mkString)
    }
    if (children.isEmpty) {
      "<%s%s />".format(name, formattedAttrs)
    } else {
      "<%s%s>%s</%s>".format(name, formattedAttrs, children.mkString, name)
    }
  }

  override def forest_text(xs: List[Any]): String = xs.mkString.replace("<", "&lt;")

  override def forest_tree(root: String): String = root

  override def treeToNode(tree: String): String = tree

}

trait ForestInScalaPkg extends ForestInScala with ListOps2InScala with ModulesInScala with JSProxyInScala