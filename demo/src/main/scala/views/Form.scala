package views

import scala.virtualization.lms.internal.ScalaCompile
import forest.lms._
import app.PersonOpsExp

object Form {

  object Ir extends ir.Form with ForestPkgExp with PersonOpsExp with ScalaCompile {

    override val codegen = new ScalaGenForestPkg { val IR: Ir.type = Ir }

    val jsCodegen = new JSGenForestPkg { val IR: Ir.type = Ir }
    jsCodegen.emitSource0(() => module[Form], "Form", new java.io.PrintWriter("target/views.js"))

    private val form = new Form {}
    def child = compile(form.child)
    def person = compile(form.person)

  }

  def child = Ir.child
  def person = Ir.person

}