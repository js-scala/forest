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
  def forest_tag(name: String, attrs: Map[String, List[Rep[Any]]], children: Rep[List[Node]]): Rep[Node]

  /**
   * Creates a text node (e.g. `foo`)
   * @param xs Elements of the text node. The type of `xs` is `List` to allow to mix string literal (e.g. `"foo"`) and symbols (e.g. `foo.bar`)
   */
  def forest_text(xs: List[Rep[Any]]): Rep[Node]

  /**
   * Creates a tree
   * @param root Tree root node
   * @param refs Additional node references
   */
  def forest_tree(root: Rep[Node], refs: Map[String, Rep[Node]]): Rep[Tree]

  def infix_root(tree: Rep[Tree]): Rep[Node]
  implicit def treeToRoot(tree: Rep[Tree]): Rep[Node] = tree.root

  def infix_ref(tree: Rep[Tree], ref: String): Rep[Node]

  implicit def nodeManifest: Manifest[Node]
  implicit def treeManifest: Manifest[Tree]
}

/**
 * Sweeter syntax
 * {{{
 *   tree(tag("div", "class"->List("article"), "data-id"->List(42))(List(
 *     tag("span")(List(text("Name: ", article.name))),
 *     tag("span")(List(text("Description: ", article.description)))
 *   )))
 * }}}
 */
trait ForestDSL extends Forest { this: ListOps2 =>

  def tag(name: String, attrs: (String, List[Rep[Any]])*)(children: Rep[List[Node]]) =
    forest_tag(name, attrs.toMap, children)

  def text(xs: Rep[Any]*) =
    forest_text(xs.toList)

  def tree(root: Rep[Node], refs: (String, Rep[Node])*) =
    forest_tree(root, refs.toMap)
}

/**
 * Forest DSL encoding as an AST
 */
trait ForestExp extends Forest with EffectExp { this: ListOps2Exp with StructExp =>

  override def forest_tag(name: String, attrs: Map[String, List[Exp[Any]]], children: Exp[List[Node]]) = {
    reflectEffect {
      children match {
        case Def(ConstList(children)) =>
          Tag(name, Left(children.toList), attrs)
        case _ =>
          Tag(name, Right(children), attrs)
      }
    }
  }

  override def forest_text(content: List[Exp[Any]]) = reflectEffect(Text(content))

  override def forest_tree(root: Exp[Node], refs: Map[String, Exp[Node]]) = struct(ClassTag[Tree]("Tree"), refs + ("root"->root))

  override def infix_root(tree: Exp[Tree]) = field[Node](tree, "root")

  override def infix_ref(tree: Exp[Tree], ref: String) = field[Node](tree, ref)


  case class Tag(name: String, children: Either[List[Exp[Node]], Exp[List[Node]]], attrs: Map[String, List[Exp[Any]]]) extends Def[Node]

  case class Text(content: List[Exp[Any]]) extends Def[Node]


  override type Node = scala.xml.Node
  override type Tree = Map[String, Node] // Because I know structs generate Scala Map
  override def nodeManifest = manifest[Node]
  override def treeManifest = manifest[Tree]
}


// --- Convenient packages

// TODO do not include JS. Use common LMS traits.
trait ForestPkg extends Forest with ForestDSL with JS with ListOps2 with Modules with JSProxyBase
trait ForestPkgExp extends ForestExp with JSExp with ListOps2Exp with ListOps2Opt with StructExp with ModulesExp with JSProxyExp