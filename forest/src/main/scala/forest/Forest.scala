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
  def forest_tag(name: String, attrs: Map[String, List[Rep[Any]]], children: Rep[List[Node]]): Rep[Node]

  /**
   * Creates a text node (e.g. `foo`)
   * @param xs Elements of the text node. The type of `xs` is `List` to allow to mix string literal (e.g. `"foo"`) and symbols (e.g. `foo.bar`)
   */
  def forest_text(xs: List[Rep[Any]]): Rep[Node]

}

/**
 * Sweeter syntax
 * {{{
 *   tag2("div", "class"->List("article"), "data-id"->List(42))(
 *     tag("span")(text("Name: ", article.name)),
 *     tag("span")(text("Description: ", article.description))
 *   )
 *   
 *   tag("ul") (for (x <- xs) yield item(x))
 * }}}
 */
trait ForestDSL extends Forest { this: ListOps =>

  def tag2(name: String, attrs: (String, List[Rep[Any]])*)(children: Rep[Node]*) =
    forest_tag(name, attrs.toMap, list_new(children))

  def tag(name: String, attrs: (String, List[Rep[Any]])*)(children: Rep[List[Node]]) =
    forest_tag(name, attrs.toMap, children)

  def text(xs: Rep[Any]*) =
    forest_text(xs.toList)

}

/**
 * Forest DSL encoding as an AST
 */
trait ForestExp extends Forest with EffectExp { this: ListOpsExp with StructExp =>

  override def forest_tag(name: String, attrs: Map[String, List[Exp[Any]]], children: Exp[List[Node]]) = {
    reflectEffect {
      children match {
        case Def(ListNew(children)) =>
          Tag(name, Left(children.toList), attrs)
        case _ =>
          Tag(name, Right(children), attrs)
      }
    }
  }

  override def forest_text(content: List[Exp[Any]]) = reflectEffect(Text(content))


  case class Tag(name: String, children: Either[List[Exp[Node]], Exp[List[Node]]], attrs: Map[String, List[Exp[Any]]]) extends Def[Node]

  case class Text(content: List[Exp[Any]]) extends Def[Node]

}


// --- Convenient packages

// TODO do not include JS. Use common LMS traits.
trait ForestPkg extends Forest with ForestDSL with IfThenElse with ListOps with Modules with JSProxyBase with Structs
trait ForestPkgExp extends ForestExp with IfThenElseExp with FunctionsExp with ListOpsExp with ListOpsExpOpt with StructExp with ModulesExp with JSProxyExp