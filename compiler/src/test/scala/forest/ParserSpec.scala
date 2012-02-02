package forest

import org.specs2.mutable.Specification
import ast._

class ParserSpec extends Specification {

  val parser = new Parser
  
  "a parser" should {
    "parse" >> {
      "a tag with just a name" >> {
        parser.parse(parser.tree(0), "div").get must equalTo (Tag("div"))
      }
      
      "tags with attributes" >> {
        parser.parse(parser.tagPrefix, """span class=foo""").get must equalTo ("span", Map("class"->List(RawText("foo"))))
        parser.parse(parser.tagPrefix, """span class="foo" id="bar"""").get must equalTo ("span", Map("class"->List(RawText("foo")), "id"->List(RawText("bar"))))
      }
      
      "spaces" >> {
        parser.parse(parser.indent(0), "  ").get must equalTo (2)
        parser.parse(parser.indent(2), "   ").get must equalTo (3)
        parser.parse(parser.indent(2), " ").isEmpty must beTrue
      }
      
      "a tree" >> {
        parser.parse(parser.tree(0), """|div
                                        | span
                                        | a
                                        |  img""".stripMargin).get must equalTo (
                                            Tag("div", List(
                                                Tag("span"),
                                                Tag("a", List(
                                                    Tag("img")
                                                ))
                                            )))
      }
      
      "a tree with arbitrary indentation length" >> {
        parser.parse(parser.tree(0), """|div
                                        |  span
                                        |       img
                                        |  div""".stripMargin).get must equalTo (
                                              Tag("div", List(
                                                  Tag("span", List(
                                                      Tag("img")
                                                  )),
                                                  Tag("div")
                                              )))
      }
      
      "parameters" >> {
        parser.parse(parser.parameters, "{article: Article}").get must equalTo (List("article"->Some("Article")))
      }
      
      "a whole document" >> {
        parser.parse(parser.document, """|{article: Article}
                                         |div class=foo
                                         |  span class="bar baz"
                                         |  a href="/yop"""".stripMargin).get must equalTo (
                                               Document(
                                                   List("article"->Some("Article")),
                                                   Tag("div", attrs=Map("class"->List(RawText("foo"))), children=List(
                                                       Tag("span", attrs=Map("class"->List(RawText("bar baz")))),
                                                       Tag("a", attrs=Map("href"->List(RawText("/yop"))))
                                                   ))
                                               )
                                             )
      }
      
      "a document using the forest expression language" >> {
        parser.parse(parser.document, """|{article}
                                         |div class={article.featured ? 'featured'}
                                         |  {for color <- article.colors}
                                         |    span
                                         |      | Color: {color}""".stripMargin).get must equalTo (
                                               Document(
                                                 List("article"->None),
                                                 Tag("div", attrs=Map("class"->List(InlineIf(Data("article.featured"), Literal("featured"), None))), children=List(
                                                   For("color", Data("article.colors"), List(
                                                     Tag("span", children=List(
                                                       Text(List(RawText("Color: "), Data("color")))
                                                     ))
                                                   ))
                                                 ))
                                               )
                                             )
      }
    }
  }
}