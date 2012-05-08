package forest.lms

import scala.virtualization.lms.common._
import scala.js._

/**
 * Simple interpreter for Forest programs
 */
trait ForestInScala extends Forest with JSInScala {

  override type Node = String
  override type Tree = String

  def tag(name: String, children: List[String], attrs: Map[String, List[Any]], ref: Option[String]): String = {
    val formattedAttrs = attrs.foldLeft(""){
      case (attrs, (name, value)) => attrs ++ " %s=\"%s\"".format(name, value.mkString)
    }
    if (children.isEmpty) {
      "<%s%s />".format(name, formattedAttrs)
    } else {
      "<%s%s>%s</%s>".format(name, formattedAttrs, children.mkString, name)
    }
  }

  def text(xs: List[Any]): String = xs.mkString

  def tree(root: String): String = root

  implicit def treeToNode(tree: String): String = tree

}