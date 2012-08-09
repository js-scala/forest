package forest

import scala.js._
import scala.virtualization.lms.common._
import scala.xml.Node

/**
 * Forest DSL interface
 */
trait Forest extends Base {

  /**
   * Creates a tag node (e.g.`<div class="foo" />`)
   * @param name Tag name (“div”, “span”, etc.)
   * @param children Nested tags
   * @param attrs Tag attributes
   */
  // FIXME attrs: Map[String, Option[Rep[String]] to handle attributes with no value
  def forest_tag(name: String, attrs: Map[String, Rep[String]], children: Rep[List[Node]]): Rep[Node]

  /**
   * Creates a text node (e.g. `foo`)
   * @param xs Elements of the text node. The type of `xs` is `List` to allow to mix string literal (e.g. `"foo"`) and symbols (e.g. `foo.bar`)
   */
  def forest_text(xs: Rep[String]): Rep[Node]

}

/**
 * Sweeter syntax
 * {{{
 *   tag('div, 'class->'article, "data-id"->article.id)(
 *     tag('span)(
 *       text("Name: " + article.name)),
 *     tag('span)(
 *       text("Description: " + article.description))
 *   )
 *   
 *   tag2('ul)(
 *     for (x <- xs) yield item(x))
 * }}}
 */
trait ForestDSL extends Forest { this: ListOps with ObjectOps =>

  // TODO replace “tag” with “el”
  def tag(name: StrValue, attrs: (StrValue, Rep[Any])*)(children: Rep[Node]*) =
    forest_tag(name.value, attrs.map({ case (n, v) => (n.value, v.toString()) }).toMap, list_new(children))

  def tag2(name: StrValue, attrs: (StrValue, Rep[Any])*)(children: Rep[List[Node]] = unit(Nil)) =
    forest_tag(name.value, attrs.map({ case (n, v) => (n.value, v.toString()) }).toMap, children)

  // TODO provide a txt"Hello $user!" equivalent to text("Hello " + user + "!")
  // TODO replace with txt("foo")
  def text(s: Rep[String]) =
    forest_text(s)

  case class StrValue(value: String)
  implicit def stringToStringValue(s: String): StrValue = StrValue(s)
  implicit def symbolToStringValue(s: Symbol): StrValue = StrValue(s.name)
  implicit def symbolToRepString(s: Symbol): Rep[String] = unit(s.name) // To be able to write "class"->'bar
  implicit def tupleWithStringValue[A <% StrValue, B <% Rep[_]](t: (A, B)): (StrValue, Rep[_]) = (t._1, t._2)
}

/**
 * Forest DSL encoding as an AST
 */
trait ForestExp extends Forest with EffectExp { this: ListOpsExp with StructExp =>

  override def forest_tag(name: String, attrs: Map[String, Exp[String]], children: Exp[List[Node]]) = {
    reflectEffect {
      children match {
        case Def(ListNew(children)) =>
          Tag(name, Left(children.toList), attrs)
        case _ =>
          Tag(name, Right(children), attrs)
      }
    }
  }

  override def forest_text(content: Exp[String]) = reflectEffect(Text(content))


  case class Tag(name: String, children: Either[List[Exp[Node]], Exp[List[Node]]], attrs: Map[String, Exp[String]]) extends Def[Node]

  case class Text(content: Exp[String]) extends Def[Node]

}


// --- Convenient packages

// TODO do not include JS. Use common LMS traits.
trait ForestPkg extends Forest with ForestDSL with IfThenElse with ListOps with StringOps with ObjectOps with Modules with JSProxyBase with Structs
trait ForestPkgExp extends ForestExp with IfThenElseExp with ListOpsExp with ListOpsExpOpt with StringOpsExp with ObjectOpsExp with ObjectOpsExpOpt with StructExp with ModulesExp with JSProxyExp