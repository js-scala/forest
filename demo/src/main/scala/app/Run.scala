package app

import java.io.PrintWriter
import forest.lms._
import js._
import views.Form

object Run extends App {

  val steve = Person(List("Bob", "Doug", "John"))
  val html = Person.edit(steve)


  val out = new PrintWriter("target/index.html")
  out.print(html)
  out.close()

  val views = new Form with ForestPkgExp with PersonOpsExp
  val jsCodegen = new JSGenForestPkg { val IR: views.type  = views }
  jsCodegen.emitSource0(() => views.Form, "Form", new PrintWriter("target/views.js"))

}