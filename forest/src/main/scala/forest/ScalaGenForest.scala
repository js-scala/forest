package forest

import scala.virtualization.lms.common._


/**
 * Scala code generator for `ForestExp` expressions
 */
trait ScalaGenForest extends ScalaGenEffect {
  val IR: ForestExp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case Tag(name, _, children, attrs) => {
      val attrsFormatted = (for ((name, value) <- attrs) yield " " + name + "={" + quote(value) + "}").mkString(" ")
      children match {
        case Left(children) => {
          if (children.isEmpty) {
            emitValDef(sym, "<%s%s />".format(name, attrsFormatted))
          } else {
            children match {
              case child :: Nil =>
                emitValDef(sym, "<%s%s>{%s}</%s>".format(name, attrsFormatted, quote(child), name))
              case _ =>
                emitValDef(sym, "<%s%s>{%s}</%s>".format(name, attrsFormatted, children.map(quote), name))
            }
          }
        }
        case Right(children) =>
          emitValDef(sym, "<%s%s>{%s}</%s>".format(name, attrsFormatted, quote(children), name))
      }
    }

    case Text(content) => {
      emitValDef(sym, "{xml.Text(%s)}".format(quote(content)))
    }

    case _ => super.emitNode(sym, node)

  }

}

trait ScalaGenForestPkg extends ScalaGenForest with ScalaGenListOps with ScalaGenObjectOps {
  val IR: ForestPkgExp
}
