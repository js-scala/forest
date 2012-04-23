package forest.lms

import virtualization.lms.common._
import virtualization.lms.internal._
import js._
import java.io.PrintWriter

/** Business class */
case class Article(name: String, price: Double, highlighted: Boolean)

/** Convenient package */
trait ForestPkg extends Forest with JS with ListOps2 with JSTraits
trait ForestPkgExp extends ForestExp with JSExp with ListOps2Exp with JSTraitsExp

// TODO routes & i18n. Don’t use JS trait. Don’t extends.
trait ArticlesDef extends ForestPkg { this: ArticleOps =>
  // TODO always use List, never use SList (but perform optimizations on generated code)
  import collection.immutable.{List => SList}

  trait Articles {

    /**
     * def show(article: Article) =
     *   <dl class="{if (article.highlighted) "highlighted" else ""}">
     *     <dt>Name</dt><dd>{article.name}</dd>
     *     <dt>Price</dt><dd>{article.price}</dd>
     *   </dl>
     */
    def show(article: Rep[Article]): Rep[Tree] = {
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
    def list(articles: Rep[List[Article]]): Rep[Tree] = {
      val items = for (article <- articles) yield {
        tag("li", List(show(article)), Map.empty, None)
      }
      tree(tag("ul", items, Map.empty, Some("articles")))
    }
  }
  implicit def __articles(a: Rep[Articles]) = repProxy(a)
  //object Articles extends Articles
}

object Main extends App with ArticlesDef with ForestPkgExp with ArticleOpsExp { self =>

  def main(article: Rep[Article]) = {
    val newArticles = register[Articles](self)
    val articles = newArticles()
    repProxy(articles).show(article)
  }

  val jsCodegen = new JSGenForest with JSGenArticleOps with JSGenTraits { val IR: self.type  = self }
  jsCodegen.emitSource(main _, "tmpl", new java.io.PrintWriter(System.out))

  val scalaCodegen = new ScalaGenForest with ScalaGenFunctions with ScalaGenArticleOps { val IR: self.type = self }
  scalaCodegen.emitSource(main _, "tmpl", new java.io.PrintWriter(System.out))

}