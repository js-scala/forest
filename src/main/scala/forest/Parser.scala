package forest

import util.parsing.combinator.JavaTokenParsers

class Parser extends JavaTokenParsers {
  
  override def skipWhitespace = false
  
  val nl: Parser[String] = "\r?\n".r
  val blankLines = nl+
  val space: Parser[String] = "[ \t]".r
  
  
  object Xml {
    val tagName: Parser[String] = """[0-9a-zA-Z]+""".r // http://www.w3.org/TR/html-markup/syntax.html#syntax-elements
    val attr: Parser[String] = """[^\s\u0000"'>/=]+""".r // http://www.w3.org/TR/html-markup/syntax.html#syntax-attributes
  }
  
  object Forest {
    val data: Parser[Data] =
      ident ^^ { Data(_) } // TODO handle paths
    val inlineIf: Parser[InlineIf] =
      (data ~ (" ? " ~> expr) ~ ((" : " ~> expr)?)) ^^ { case cond ~ thenPart ~ elsePart => InlineIf(cond, thenPart, elsePart) }
    val literal: Parser[Literal] =
      ('"' ~> (not('"')+) <~ '"') ^^ { cs => Literal(cs.mkString) }
  
    val expr: Parser[Expr] =
      data | inlineIf | literal
  }
  
  // Forest expression (enclosed in braces)
  val expr: Parser[Expr] =
    "{" ~> Forest.expr <~ "}"
  
  // HTML text
  // TODO handle `until` character escape
  def rawText(until: Char): Parser[RawText] =
    (not(until)+) ^^ { cs => RawText(cs.mkString) }
  
  // Mix of raw text and forest expressions
  def textContent(until: Char): Parser[List[TextContent]] =
    (rawText(until) | expr)+
  
  // http://www.w3.org/TR/html-markup/syntax.html#syntax-attr-unquoted
  // TODO handle character references
  val unquotedValue: Parser[List[TextContent]] =
    (("""[^\s\u0000"'=<>`]+""".r ^^ { RawText(_) }) | expr) ^^ { List(_) }
  
  val quotedValue: Parser[List[TextContent]] =
    ('"' ~> textContent('"') <~ '"') | ("'" ~> textContent('\'') <~ "'")
  
  val attrValue: Parser[List[TextContent]] =
    unquotedValue | quotedValue | (success() ^^^ Nil)
  
  val attrs: Parser[Map[String, List[TextContent]]] =
    repsep(
        (Xml.attr ~ attrValue) ^^ { case key ~ value => (key -> value) },
        space+
    ) ^^ { as => as.toMap }
  
  // Beginning of a HTML tag: name and attributes
  val tag: Parser[(String,Map[String, List[TextContent]])] =
    (Xml.tagName ~ ((space ~> attrs)?)) ^^ { case name ~ attrs => (name, attrs.getOrElse(Map.empty)) }
  
  // Expects at least `n` consecutive spaces. Returns the number of spaces.
  def indent(n: Int): Parser[Int] =
    (repN(n, space) ~> (space*)) ^^ { s => n + s.size }
  
  // From a given indentation value `n`, expects a tag, a blank line and children tags at a depth of `n + 1`
  def tree(n: Int): Parser[Tag] = Parser { in =>
    indent(n)(in) flatMapWithNext { depth =>
      (tag ~ ((blankLines ~> tree(depth + 1))*)) ^^ { case (name, attrs) ~ children => Tag(name, children, attrs) }
    }
  }
  
  val parameter: Parser[(String, String)] =
    (ident ~ (": " ~> ident)) ^^ { case name ~ kind => (name -> kind) }
  
  // Template parameters
  val parameters: Parser[Map[String, String]] =
    ('{' ~> repsep(parameter, ", ") <~ '}') ^^ { _.toMap }
  
  val document: Parser[Document] =
    (parameters ~ blankLines ~ tree(0)) ^^ { case p ~ _ ~ t => Document(p, t) }
  
}
