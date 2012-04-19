package forest.lms

import virtualization.lms.common._
import virtualization.lms.internal._
import js._
import java.io.PrintWriter

/** Business class */
case class Article(name: String, price: Double, highlighted: Boolean)

// TODO routes & i18n. Don’t use JS trait
trait Templates { this: Forest with JS with ListOps2 with ArticleOps =>
  // TODO always use List, never use SList (but perform optimizations on generated code)
  import collection.immutable.{List => SList}

  def apply(articles: Rep[List[Article]]) = {

  /**
   * def show(article: Article) =
   *   <dl class="{if (article.highlighted) "highlighted" else ""}">
   *     <dt>Name</dt><dd>{article.name}</dd>
   *     <dt>Price</dt><dd>{article.price}</dd>
   *   </dl>
   */
  val show: Rep[Article => Tree] = fun { article: Rep[Article] =>
    val name = List(
        tag("dt", List(text("Name")), Map.empty, None),
        tag("dd", List(text(article.name)), Map("class"->SList("name")), None)
    )
    val price = List(
        tag("dt", List(text("Price")), Map.empty, None),
        tag("dd", List(text(article.price, " Euros")), Map.empty, None)
    )
    //tree(tag("dl", name ++ price, Map("class"->(if(article.highlighted) List("highlighted") else List(""))), None))
    tree(tag("dl", name ++ price, Map("class"->SList("article")), Some("article")))
  }

  /**
   * def list(articles: List[Article]) =
   *   <ul>
   *     for (article <- articles) yield {
   *       <li>{show(article)}</li>
   *     }
   *   </ul>
   */
  val list: Rep[List[Article] => Tree] = fun { articles: Rep[List[Article]] =>
    val items = for (article <- articles) yield {
      tag("li", List(show(article)), Map.empty, None)
    }
    tree(tag("ul", items, Map.empty, Some("articles")))
  }

    list(articles)
  }
}

object Main extends App {

  new Templates with JSExp with ListOps2Exp with ForestExp with ArticleOpsExp { self =>
    val codegen = new ForestJSCodegen with JSGenArticleOps { val IR: self.type = self }
    codegen.emitSource(apply _, "tmpl", new java.io.PrintWriter(System.out))
  }

  // TODO Remove JSExp ^^
  new Templates with JSExp with ListOps2Exp with ForestExp with ArticleOpsExp { self =>
    val codegen = new ForestScalaCodegen with ScalaGenFunctions with ScalaGenArticleOps { val IR: self.type = self }
    codegen.emitSource(apply _, "tmpl", new java.io.PrintWriter(System.out))
  }

}