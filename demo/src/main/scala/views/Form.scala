package views

import scala.virtualization.lms.internal.ScalaCompile
import forest._

object Form {

  object Ir extends ir.Form with ForestStringPkgExp with ScalaCompile with FieldsExp {

    override val codegen = new ScalaGenForestPkg with ScalaGenFields { val IR: Ir.type = Ir }

    val jsCodegen = new JSGenForestPkg with JSGenFields { val IR: Ir.type = Ir }
    jsCodegen.emitModule(module[Form], "Form", new java.io.PrintWriter("target/views.js"))

    def person = compile((new Form {}).person)

  }

  def person = Ir.person

}