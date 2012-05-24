package forest.lms

import virtualization.lms.common._
import js._

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
   * @param ref Identifier of this node
   */
  def tag(name: String, children: Rep[List[Node]], attrs: Map[String, List[Rep[Any]]], ref: Option[String]): Rep[Node]

  /**
   * Creates a text node (e.g. `foo`)
   * @param xs Elements of the text node. The type of `xs` is `List` to allow to mix string literal (e.g. `"foo"`) and symbols (e.g. `foo.bar`)
   */
  def text(xs: List[Rep[Any]]): Rep[Node]
  def text(xs: Rep[Any]*): Rep[Node] = text(xs.toList) // Convenient overload

  /**
   * Creates a tree from a given node
   * Isn’t a `Node` already a `Tree`? In Scala, yes. In JavaScript, no: a `Tree` is an object containing references to some nodes.
   * @param root Tree root node
   */
  def tree(root: Rep[Node]): Rep[Tree] // FIXME How to force `root` to be a Rep[Tag]?
  implicit def treeToNode(tree: Rep[Tree]): Rep[Node]

  // HACK : Sometimes I need to provide a Manifest for nodes or tree
  implicit def nodeManifest[T <: Node]: Manifest[T] = manifest[AnyRef].asInstanceOf[Manifest[T]]
  implicit def treeManifest[T <: Tree]: Manifest[T] = manifest[AnyRef].asInstanceOf[Manifest[T]]
}

/**
 * Forest DSL encoding as an AST
 */
trait ForestExp extends Forest with BaseExp { this: EffectExp with ListOps2Exp =>

  override def tag(name: String, children: Exp[List[Node]], attrs: Map[String, List[Exp[Any]]], ref: Option[String]) = Tag(name, children, attrs, ref)
  override def text(content: List[Exp[Any]]) = Text(content)

  override def tree(root: Exp[Node]) = Tree(root)

  def treeToNode(tree: Exp[Tree]): Exp[Node] = TreeRoot(tree)

  case class TreeRoot(tree: Exp[Tree]) extends Def[Node]

  case class Tag(name: String, children: Exp[List[Node]], attrs: Map[String, List[Exp[Any]]], ref: Option[String]) extends Def[Node]
  case class Text(content: List[Exp[Any]]) extends Def[Node]

  case class Tree(root: Exp[Node]) extends Def[Tree] {
    lazy val refs: Map[String, Exp[Node]] = {
      def collectRefs(rootNode: Exp[Node]): Map[String, Exp[Node]] = {
        def collectRefs2(ref: Option[String], childrenExp: Exp[List[Node]]): Map[String, Exp[Node]] = {
          val children = childrenExp match {
            case Def(ConstList(children)) => children
            case _ => Nil
          }
          ref.map(_ -> rootNode).toMap ++ children.flatMap(collectRefs)
        }
        rootNode match {
          case Def(Tag(_, children, _, ref)) => collectRefs2(ref, children)
          case Def(Reflect(Tag(_, children, _, ref), _, _)) => collectRefs2(ref, children)
          case Def(Text(_)) | Def(Reflect(Text(_), _, _)) => Map.empty // We don’t care of text node for now
          case _ => sys.error("Don’t do that again. Please.")
        }
      }
      collectRefs(root)
    }
  }


  override def syms(x: Any) = x match {
    case Tag(_, children, attrs, _) => (attrs.values.flatten.flatMap(syms) ++ syms(children)).toList
    case _ => super.syms(x)
  }

  override def symsFreq(x: Any) = x match {
    case Tag(_, children, attrs, _) => (attrs.values.flatten.flatMap(freqNormal) ++ freqNormal(children)).toList
    case _ => super.symsFreq(x)
  }

}


// --- Convenient packages

trait ForestPkg extends Forest with JS with ListOps2 with Modules with JSProxyBase
trait ForestPkgExp extends ForestExp with JSExp with ListOps2Exp with ListOps2Opt with ModulesExp with JSProxyExp with FieldsExp