package forest.lms

import virtualization.lms.common._
import virtualization.lms.internal._
import js._
import java.io.PrintWriter

// --- Business class definition

case class Article(name: String, price: Double, highlighted: Boolean)


// --- Convenient packages

trait ForestPkg extends Forest with JS with ListOps2 with Modules with JSProxyBase
trait ForestPkgExp extends ForestExp with JSExp with ListOps2Exp with ListOps2Opt with ModulesExp with JSProxyExp

// --- Example of template definition

// TODO routes & i18n. Don’t use JS trait. Don’t use extends (ok, extending is mandatory to share the implicit scope…).
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
  def Articles = module[Articles] // TODO I’d like to just write “object Articles extends Articles”
}

// --- Usage

object Main extends App {

  object JSProg extends ArticlesDef with ForestPkgExp with ArticleOpsExp {
    def main(article: Rep[Article]) = {
      Articles.show(article)
    }
  }

  // The JavaScript code generation
  val jsCodegen = new JSGenForest with JSGenArticleOps with JSGenModules with JSGenProxy { val IR: JSProg.type  = JSProg }
  jsCodegen.emitSource(JSProg.main _, "tmpl", new java.io.PrintWriter(System.out))

  // The Scala code generation (FIXME really needed? Why not follow the “InScala” way? I think code generation allows to perform more optimizations)
  /*val scalaCodegen = new ScalaGenForest with ScalaGenFunctions with ScalaGenArticleOps with ScalaGenModules { val IR: self.type = self }
  scalaCodegen.emitSource(main _, "tmpl", new java.io.PrintWriter(System.out))*/

  object ScalaProg extends ArticlesDef with ForestInScala with ArticleOpsInScala with ModulesInScala with JSProxyInScala with ListOps2InScala {

    override def create[A : Manifest]: A = {
      if (manifest[A] equals manifest[Articles]) (new Articles {}).asInstanceOf[A]
      else super.create[A]
    }

    def main(articles: List[Article]) {
      println(Articles.list(articles))
    }

  }

  ScalaProg.main(List(Article("Something", 42.0, false)))

}