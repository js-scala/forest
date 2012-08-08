package forest

import java.io.PrintWriter
import scala.js._
import scala.virtualization.lms.common._


// --- Example of template definition

// TODO routes & i18n.
trait Articles extends ForestPkg with LiftAll {
  import collection.immutable.{List => SList}

  type Article = Record { val name: String; val price: Double; val highlighted: Boolean }
  def Article(n: Rep[String], p: Rep[Double], h: Rep[Boolean]): Rep[Article] =
    new Record { val name = n; val price = p; val highlighted = h }

  trait Articles {

    /**
     * def show(article: Article) =
     *   <dl class="{if (article.highlighted) "highlighted" else ""}">
     *     <dt>Name</dt><dd>{article.name}</dd>
     *     <dt>Price</dt><dd>{article.price}</dd>
     *   </dl>
     */
    def show(article: Rep[Article]) = {
      tag("dl", "class"->SList(if (article.highlighted) "highlighted" else ""))(
        tag("dt")(text("Name")),
        tag("dd")(text(article.name)),
        tag("dt")(text("Price")),
        tag("dd")(text(article.price, " Euros"))
      )
    }

    /**
     * def list(articles: List[Article]) =
     *   <ul>
     *     for (article <- articles) yield {
     *       <li>{show(article)}</li>
     *     }
     *   </ul>
     */
    def list(articles: Rep[List[Article]]) = {
        val root = tag2("ul")(
          for (article <- articles) yield {
            tag("li")(show(article))
          }
        )
        root
    }
  }
  def Articles = module[Articles] // TODO I’d like to just write “object Articles extends Articles”
}

// --- Usage

object Main extends App {

  object Prog extends Articles with ForestPkgExp {
    val articles = new Articles {}
  }

  // The JavaScript code generation
  val jsCodegen = new JSGenForestPkg { val IR: Prog.type  = Prog }
  jsCodegen.emitModule(Prog.Articles, "Articles", new PrintWriter(System.out))

  val scalaGen = new ScalaGenForestPkg { val IR: Prog.type = Prog }
  scalaGen.emitSource(Prog.articles.list, "ArticlesList", new PrintWriter(System.out))
}
