package forest

import java.io.PrintWriter
import scala.js._
import scala.virtualization.lms.common._


// --- Example of template definition

// TODO routes & i18n.
trait Articles extends ForestPkg with ScalaOpsPkg with LiftScala with Structs {

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
    tag('dl, 'class->(if (article.highlighted) "highlighted" else ""))(
      tag('dt)(text("Name")),
      tag('dd)(text(article.name)),
      tag('dt)(text("Price")),
      tag('dd)(text(article.price + " Euros"))
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
      tag2('ul)(
        for (article <- articles) yield {
          tag('li)(show(article))
        }
      )
  }
}

// --- Usage

object Main extends App {

  val prog = new Articles with ForestPkgExp with ScalaOpsPkgExp with TupledFunctionsExp with StructExp

  // The JavaScript code generation
  val jsCodegen = new JSGenForestPkg with JSCodeGenPkg with JSGenStruct { val IR: prog.type = prog }
  jsCodegen.emitSource(prog.list, "ArticlesList", new PrintWriter(System.out))

  val scalaGen = new ScalaGenForestPkg with ScalaCodeGenPkg with ScalaGenStruct { val IR: prog.type = prog }
  scalaGen.emitSource(prog.list, "ArticlesList", new PrintWriter(System.out))
}
