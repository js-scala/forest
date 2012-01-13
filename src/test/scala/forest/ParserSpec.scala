package forest

import org.specs2.mutable.Specification

class ParserSpec extends Specification {

  val parser = new Parser
  
  "a parser" should {
    "parse" >> {
      "a tag with just a name" >> {
        parser.parse(parser.tree(0), "div").get must be_== (Tag("div"))
      }
      
      "tags with attributes" >> {
        parser.parse(parser.tag, """span class="foo"""").get must be_== ("span", Map("class"->List(Literal("foo"))))
        parser.parse(parser.tag, """span class="foo" id="bar"""").get must be_== ("span", Map("class"->List(Literal("foo")), "id"->List(Literal("bar"))))
      }
      
      "spaces" >> {
        parser.parse(parser.indent(0), "  ").get must be_== (2)
        parser.parse(parser.indent(2), "   ").get must be_== (3)
        parser.parse(parser.indent(2), " ").isEmpty must beTrue
      }
      
      "a tree" >> {
        parser.parse(parser.tree(0), """|div
                                        | span
                                        | a
                                        |  img""".stripMargin).get must be_== (
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
                                        |  div""".stripMargin).get must be_== (
                                              Tag("div", List(
                                                  Tag("span", List(
                                                      Tag("img")
                                                  )),
                                                  Tag("div")
                                              )))
      }
      
      "parameters" >> {
        parser.parse(parser.parameters, "{article: Article}").get must be_== (Map("article"->"Article"))
      }
      
      /*"a whole document" >> {
        parser.parse(parser.document, """"|{article: Article}
                                          |div class="foo"
                                          |  span class="bar baz"
                                          |  a href="/yop"""".stripMargin).get must be_== (
                                                Document(
                                                    Map("article"->"Article"),
                                                    Node(Tag("div", Map("class"->"foo")), List(
                                                          Node(Tag("span", Map("class"->"bar baz"))),
                                                          Node(Tag("a", Map("href"->"/yop")))
                                                        )
                                                ))
                                              )
      }*/
    }
  }
}