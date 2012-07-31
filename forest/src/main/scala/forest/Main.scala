package forest

import java.io.PrintWriter
import scala.js._
import scala.virtualization.lms.common._

// --- Business class definition

case class Article(name: String, price: Double, highlighted: Boolean)

trait ArticleOps extends Base with Fields {
  class ArticleOps(implicit article: Rep[Article]) extends Fields[Article] {
    def name = field[String]("name")
    def price = field[Double]("price")
    def highlighted = field[Boolean]("highlighted")
  }
  implicit def repArticleToOps(article: Rep[Article]) = new ArticleOps()(article)
}


// --- Example of template definition

// TODO routes & i18n.
trait Articles extends ForestPkg with ArticleOps {
  import collection.immutable.{List => SList}

  trait Articles {

    /**
     * def show(article: Article) =
     *   <dl class="{if (article.highlighted) "highlighted" else ""}">
     *     <dt>Name</dt><dd>{article.name}</dd>
     *     <dt>Price</dt><dd>{article.price}</dd>
     *   </dl>
     */
    def show(article: Rep[Article]) = {
      tag("dl", "class"->SList(if (article.highlighted) "highlighted" else ""))(List(
        tag("dt")(List(text("Name"))),
        tag("dd")(List(text(article.name))),
        tag("dt")(List(text("Price"))),
        tag("dd")(List(text(article.price, " Euros")))
      ))
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
      tree(tag("ul")(
        for (article <- articles) yield {
          tag("li")(List(show(article)))
        }
      ))
    }
  }
  def Articles = module[Articles] // TODO I’d like to just write “object Articles extends Articles”
}

// --- Usage

object Main extends App {

  object JSProg extends Articles with ForestPkgExp with FieldsExp

  // The JavaScript code generation
  val jsCodegen = new JSGenForestPkg with JSGenFields { val IR: JSProg.type  = JSProg }
  jsCodegen.emitModule(JSProg.Articles, "Articles", new PrintWriter("target/show-article.js"))

  /*val scalaCodegen = new ScalaGenForest with ScalaGenFunctions with ScalaGenArticleOps with ScalaGenModules { val IR: self.type = self }
  scalaCodegen.emitSource(main _, "tmpl", new java.io.PrintWriter(System.out))*/

  object ScalaProg extends Articles with ForestInScalaPkg with FieldsInScala {

    override def create[A : Manifest]: A = {
      if (manifest[A] equals manifest[Articles]) (new Articles {}).asInstanceOf[A]
      else super.create[A]
    }

    def main(articles: List[Article]) {
      val target = new PrintWriter("target/index.html")
      target.println("""<!DOCTYPE html>
        <html>
          <head></head>
          <body>
            <h1>Articles</h1>
            %s
            <button type="button" id="add-article">Add an article</button>
            <button type="button" id="fresh-list">Fresh list</button>
            <script type="text/javascript" src="show-article.js"></script>
            <script type="text/javascript">
              (function () {
                var addBtn = document.getElementById('add-article');
                addBtn.onclick = function () {
                  var articleTree = Articles.show({
                    name: 'Item',
                    price: 123.45,
                    highlighted: false
                  });
                  var li = document.createElement('li');
                  li.appendChild(articleTree);
                  document.querySelector('ul').appendChild(li);
                };
                var freshBtn = document.getElementById('fresh-list');
                freshBtn.onclick = function () {
                  var tree = Articles.list([{
                    name: 'Item 1',
                    price: 12.34,
                    highlighted: true
                  }, {
                    name: 'Item 2',
                    price: 23.45,
                    highlighted: false
                  }]);
                  var ul = document.querySelector('ul');
                  ul.parentNode.replaceChild(tree.root, ul);
                };
              })();
            </script>
          </body>
        </html>""".format(Articles.list(articles)("root")))
      target.close()
    }

  }

  ScalaProg.main(List(Article("Something", 42.0, false), Article("Foobar<b>baz</b>", 0, true)))

}