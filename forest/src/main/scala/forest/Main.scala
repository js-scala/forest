package forest

import java.io.PrintWriter
import scala.js.language.JsScala
import scala.js.exp.JsScalaExp
import scala.virtualization.lms.common._


// --- Example of template definition

// TODO routes & i18n.
trait Articles extends JsScala with Forest {

  type Article = Record { val name: String; val price: Double; val highlighted: Boolean }
  def Article(n: Rep[String], p: Rep[Double], h: Rep[Boolean]): Rep[Article] =
    new Record { val name = n; val price = p; val highlighted = h }

  /**
   * def show(article: Article) =
   *   <dl class="{if (article.highlighted) "highlighted" else ""}">
   *     <dt>Name</dt><dd>{article.name}</dd>
   *     <dt>Price</dt><dd>{article.price}</dd>
   *   </dl>
   */
  def show(article: Rep[Article]) = {
    el('dl, 'class->(if (article.highlighted) "highlighted" else ""))(
      el('dt)(txt("Name")),
      el('dd)(txt(article.name)),
      el('dt)(txt("Price")),
      el('dd)(txt(article.price + " Euros"))
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
      el('ul)(
        for (article <- articles) yield {
          el('li)(show(article))
        }
      )
  }
}

// --- Usage

object Main extends App {

  val prog = new Articles with JsScalaExp with ForestExp

  // The JavaScript code generation
  val jsCodegen = new scala.js.gen.js.GenJsScala with JSGenForest { val IR: prog.type = prog }
  jsCodegen.emitSource(prog.list, "ArticlesList", new PrintWriter(System.out))

  val scalaGen = new scala.js.gen.scala.GenJsScala with ScalaGenForest { val IR: prog.type = prog }
  scalaGen.emitSource(prog.list, "ArticlesList", new PrintWriter(System.out))
}
