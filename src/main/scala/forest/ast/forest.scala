package forest.ast

import scala.util.parsing.input.Positional
import collection.SortedMap

/**
 * Root object of the AST.
 */
case class Document(parameters: List[(String, Option[String])], tree: Node) extends Positional

sealed abstract class Node

/**
 * HTML tag optionally defining attributes whose values can be dynamic
 * 
 * {{{
 *   div class={user.isAdmin ? 'admin' : 'user'} @user
 * }}}
 */
case class Tag(
    name: String,
    children: List[Node] = Nil,
    attrs: Map[String, List[TextContent]] = Map.empty,
    ref: Option[String] = None
) extends Node with Positional

/**
 * A Node containing raw text or expressions mixed together
 * 
 * Example:
 * 
 * {{{
 *   | Hello {user.name}
 * }}}
 */
case class Text(content: List[TextContent]) extends Node with Positional // FIXME Add a node ref?



sealed abstract class TextContent

/**
 * {{{
 *   | This is a raw text content
 * }}}
 */
case class RawText(text: String) extends TextContent with Positional

/**
 * Base class for forest expressions.
 * 
 * {{{
 *   {foo.bar}
 *   {'foo'}
 *   {foo.isBar ? foo.baz : 'bah'}
 * }}}
 */
sealed abstract class Expr extends TextContent

/**
 * {{{
 *   {foo.bar.baz}
 * }}}
 */
case class Data(path: String) extends Expr with Positional

/**
 * {{{
 *   {foo.isBar ? foo.baz : 'bah'}
 * }}}
 */
case class InlineIf(cond: Data, thenPart: Expr, elsePart: Option[Expr]) extends Expr with Positional

/**
 * {{{
 *   {'foo'}
 *   {"bar"}
 * }}}
 */
case class Literal(value: String) extends Expr with Positional

/**
 * Loop
 * 
 * Example:
 * 
 * {{{
 *   {for user <- users}
 *     li | {user.name}
 * }}}
 */
case class For(it: String, seq: Data, body: List[Node]) extends Node with Positional

/**
 * Condition (may have a else part)
 * 
 * {{{
 *   {if user.isAdmin}
 *     div class=user
 *       | {user.name}
 *   {else}
 *     div | Guest
 * }}}
 */
case class If(cond: Expr, thenPart: List[Node], elsePart: Option[List[Node]] = None) extends Node with Positional

/**
 * Subtemplate call
 * 
 * {{{
 *   {for user <- users}
 *     li
 *       {call views.user(user)}
 * }}}
 */
case class Call(callee: String, args: List[Expr]) extends Node with Positional // FIXME quid template inheritance?
