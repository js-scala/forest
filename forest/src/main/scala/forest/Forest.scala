package forest

import scala.virtualization.lms.common._
import scala.js.language.dom.ElementOps
import scala.js.exp.dom.ElementOpsExp

/**
 * Forest DSL interface
 */
trait Forest extends ElementOps with ListOps with ObjectOps {
  import language.implicitConversions
  import Magnets._

  /**
   * Creates a tag node (e.g.`<div class="foo" />`)
   * @param name Tag name (“div”, “span”, etc.)
   * @param children Nested tags
   * @param attrs Tag attributes
   */
  // FIXME attrs: Map[String, Option[Rep[String]] to handle attributes with no value
  def forest_tag(name: String, attrs: Map[String, Rep[String]], children: Rep[List[Element]], xmlns: String): Rep[Element]

  /**
   * Creates a text node (e.g. `foo`)
   * @param xs Elements of the text node. The type of `xs` is `List` to allow to mix string literal (e.g. `"foo"`) and symbols (e.g. `foo.bar`)
   */
  def forest_text(xs: Rep[String]): Rep[Element]

  /**
   * Sweeter syntax
   * {{{
   *   el('div, 'class->'article, "data-id"->article.id)(
   *     el('span)(
   *       "Name: " + article.name),
   *     el('span)(
   *       "Description: " + article.description)
   *   )
   *
   *   el('ul)(
   *     for (x <- xs) yield item(x)
   *   )
   * }}}
   */
  def el(name: StrValue, attrs: (StrValue, Rep[_])*)(children: NodeValue*)(implicit ns: NS) = {
    val constNodes = children.collect { case ConstNode(node) => node }
    val cs = if (constNodes.size == children.size) list_new(constNodes.to[List]) else {
      children.foldLeft(List[Element]()) {
        case (cs, ConstNode(n)) => n :: cs
        case (cs, NodeList(ns)) => ns ++ cs
      }
    }
    forest_tag(name.value, attrs.map({ case (n, v) => (n.value, v.toString()) }).toMap, cs, ns.value)
  }

  def txt(s: Rep[String]) =
    forest_text(s)

  /**
   * {{{
   *   el('div)(
   *     withNamespace(NS.SVG) { implicit ns =>
   *       el('svg)(
   *         el('defs)(…),
   *         el('g)(…))
   *     })
   * }}}
   */
  def withNamespace(ns: NS)(f: NS => Rep[Element]): Rep[Element] = f(ns)

  case class NS(value: String)
  object NS {
    implicit val HTML = NS("http://www.w3.org/1999/xhtml")
    val SVG = NS("http://www.w3.org/2000/svg")
    val MATHML = NS("http://www.w3.org/1998/Math/MathML")
  }

  implicit def symbolToRepString(s: Symbol): Rep[String] = unit(s.name) // To be able to write "class"->'bar

  object Magnets {
    case class StrValue(value: String)
    implicit def stringToStringValue(s: String): StrValue = StrValue(s)
    implicit def symbolToStringValue(s: Symbol): StrValue = StrValue(s.name)
    implicit def tupleWithStringValue[A <% StrValue, B <% Rep[_]](t: (A, B)): (StrValue, Rep[_]) = (t._1, t._2)

    sealed abstract class NodeValue
    case class ConstNode(node: Rep[Element]) extends NodeValue
    case class NodeList(nodes: Rep[List[Element]]) extends NodeValue
    implicit def repNode(node: Rep[Element]): NodeValue = ConstNode(node)
    implicit def repNodes(nodes: Rep[List[Element]]): NodeValue = NodeList(nodes)
    implicit def repText(s: Rep[String]): NodeValue = ConstNode(txt(s))
    implicit def string(s: String)(implicit ev: String => Rep[String]): NodeValue = repText(s)
  }
}

/**
 * Forest DSL encoding as an AST
 */
trait ForestExp extends Forest with EffectExp with ElementOpsExp with ListOpsExp with ObjectOpsExp {

  override def forest_tag(name: String, attrs: Map[String, Exp[String]], children: Exp[List[Element]], xmlns: String) = {
    reflectEffect {
      children match {
        case Def(ListNew(cs)) =>
          Tag(name, xmlns, Left(cs.toList), attrs)
        case _ =>
          Tag(name, xmlns, Right(children), attrs)
      }
    }
  }

  override def forest_text(content: Exp[String]) = reflectEffect(Text(content))


  case class Tag(name: String, xmlns: String, children: Either[List[Exp[Element]], Exp[List[Element]]], attrs: Map[String, Exp[String]]) extends Def[Element]

  case class Text(content: Exp[String]) extends Def[Element]

}
