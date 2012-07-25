package forest

import scala.js._
import scala.virtualization.lms.common._

/**
 * Forest DSL interface
 */
trait Forest extends Base {

  type Node
  type Tree

  /**
   * Creates a tag node (e.g.`<div class="foo" />`)
   * @param name Tag name (“div”, “span”, etc.)
   * @param children Nested tags
   * @param attrs Tag attributes
   */
  def forest_tag(name: String, attrs: Map[String, List[Rep[Any]]], children: Rep[List[Node]], ref: Option[String]): Rep[Node]

  /**
   * Creates a text node (e.g. `foo`)
   * @param xs Elements of the text node. The type of `xs` is `List` to allow to mix string literal (e.g. `"foo"`) and symbols (e.g. `foo.bar`)
   */
  def forest_text(xs: List[Rep[Any]]): Rep[Node]

  def forest_tree(root: Rep[Node]): Rep[Tree]
  implicit def treeToNode(tree: Rep[Tree]): Rep[Node]

  implicit def nodeManifest: Manifest[Node]
  implicit def treeManifest: Manifest[Tree]
}

/**
 * Sweeter syntax
 * {{{
 *   tag("div", "class"->List("article"), "data-id"->List(42))(List(
 *     tag("span")(List(text("Name: ", article.name))),
 *     tag("span")(List(text("Description: ", article.description)))
 *   ))
 * }}}
 */
trait ForestDSL extends Forest { this: ListOps2 =>

  def tag(name: String, attrs: (String, List[Rep[Any]])*)(children: Rep[List[Node]]) =
    forest_tag(name, attrs.toMap, children, None)

  def text(xs: Rep[Any]*) =
    forest_text(xs.toList)

  def tree(root: Rep[Node]) =
    forest_tree(root)
}

/**
 * Forest DSL encoding as an AST
 */
trait ForestExp extends Forest with EffectExp { this: ListOps2Exp =>

  override def forest_tag(name: String, attrs: Map[String, List[Exp[Any]]], children: Exp[List[Node]], ref: Option[String]) = {
    reflectEffect {
      children match {
        case Def(ConstList(children)) =>
          Tag(name, Left(children.toList), attrs, ref)
        case _ =>
          Tag(name, Right(children), attrs, ref)
      }
    }
  }

  override def forest_text(content: List[Exp[Any]]) = reflectEffect(Text(content))

  override def forest_tree(root: Exp[Node]) = ForestTree(root)

  override def treeToNode(tree: Exp[Tree]): Exp[Node] = TreeRoot(tree)

  case class TreeRoot(tree: Exp[Tree]) extends Def[Node]

  case class Tag(name: String, children: Either[List[Exp[Node]], Exp[List[Node]]], attrs: Map[String, List[Exp[Any]]], ref: Option[String]) extends Def[Node]

  case class Text(content: List[Exp[Any]]) extends Def[Node]

  case class ForestTree(root: Exp[Node]) extends Def[Tree] {
    lazy val refs: Map[String, Exp[Node]] = {
      def collectRefs(rootNode: Exp[Node]): Map[String, Exp[Node]] = {
        def collectRefs2(ref: Option[String], children: Seq[Exp[Node]]): Map[String, Exp[Node]] =
          ref.map(_ -> rootNode).toMap ++ children.flatMap(collectRefs)
        val extractChildren: Either[List[Exp[Node]], Exp[List[Node]]] => Seq[Exp[Node]] = _ match {
          case Left(children) => children
          case Right(childrenExp) => Nil
        }
        rootNode match {
          case Def(Tag(_, children, _, ref)) => collectRefs2(ref, extractChildren(children))
          case Def(Reflect(Tag(_, children, _, ref), _, _)) => collectRefs2(ref, extractChildren(children))
          case Def(Text(_)) | Def(Reflect(Text(_), _, _)) => Map.empty // We don’t care of text node for now
          case _ => sys.error("Don’t do that again. Please.")
        }
      }
      collectRefs(root)
    }
  }

}

/**
 * Represents HTML with scala XML standard library
 */
trait ForestXmlExp extends ForestExp { this: ListOps2Exp =>
  override type Node = scala.xml.Node
  override type Tree = scala.xml.Node

  override def nodeManifest = manifest[Node]
  override def treeManifest = manifest[Tree]
}

/**
 * Represents HTML with text
 */
trait ForestStringExp extends ForestExp { this: ListOps2Exp =>
  override type Node = String
  override type Tree = String

  override def nodeManifest = manifest[Node]
  override def treeManifest = manifest[Tree]
}

// --- Convenient packages

trait ForestPkg extends Forest with ForestDSL with JS with ListOps2 with Modules with JSProxyBase
trait ForestPkgExp extends ForestExp with JSExp with ListOps2Exp with ListOps2Opt with ModulesExp with JSProxyExp
trait ForestStringPkgExp extends ForestPkgExp with ForestStringExp
trait ForestXmlPkgExp extends ForestPkgExp with ForestXmlExp