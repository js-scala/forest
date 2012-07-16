package app

import java.io.PrintWriter
import forest._
import js._

object Run extends App {

  val steve = Person(List("Bob", "Doug", "John"))
  val html = Person.edit(steve)


  val out = new PrintWriter("target/index.html")
  out.print(html)
  out.close()

}