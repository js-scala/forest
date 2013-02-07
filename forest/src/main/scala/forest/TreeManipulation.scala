package forest

import scala.js.JSGenEffect
import scala.virtualization.lms.common._
import scala.xml.Node

/**
 * DSL to manipulate trees.
 */
trait TreeManipulation extends Forest with ListOps with ObjectOps {

  /** A set of Nodes to be manipulated */
  type NodeRef
  type NodeSelection
  type TransformableNode

  /**
   * Transforms the node referenced by `r` according to the transformation `t`
   */
  def infix_transform(r: Rep[NodeRef], t: Rep[TransformableNode] => Rep[Unit]): Rep[Unit]

  /** Finds the nodes matching the CSS3 `selector` */
  def infix_find(root: Rep[TransformableNode], selector: Rep[String]): Rep[NodeSelection]

  /**
   * Appends `node` to each node of `target` node selection
   */
  def infix_append(target: Rep[NodeSelection], node: Rep[Node]): Rep[Unit]

  implicit def nodeRefManifest: Manifest[NodeRef]
  // implicit def nodeSelectionManifest: Manifest[NodeSelection]
  implicit def transformableNodeManifest: Manifest[TransformableNode]
}

trait TreeManipulationExp extends TreeManipulation with EffectExp with ListOpsExp with ObjectOpsExp { this: ForestExp =>

  override type TransformableNode = scala.xml.NodeSeq
  override type NodeSelection = scala.xml.NodeSeq
  override type NodeRef = forest.lib.NodeRef

  override def nodeRefManifest = manifest[NodeRef]
  override def transformableNodeManifest = manifest[TransformableNode]

  case class NodeRefTransform(root: Exp[NodeRef], o: Sym[org.fusesource.scalate.scuery.Transformer], n: Sym[TransformableNode], body: Block[Unit]) extends Def[Unit]
  case class TransformableFind(root: Rep[TransformableNode], selector: Rep[String]) extends Def[NodeSelection]
  case class SelectionAppend(target: Rep[NodeSelection], node: Rep[Node]) extends Def[Unit]

  override def infix_transform(r: Rep[NodeRef], t: Exp[TransformableNode] => Exp[Unit]) = {
    val o = fresh[org.fusesource.scalate.scuery.Transformer]
    val n = fresh[TransformableNode]
    val b = reifyEffects(t(n))
    reflectEffect(NodeRefTransform(r, o, n, b), summarizeEffects(b))
  }

  override def infix_find(root: Rep[TransformableNode], selector: Rep[String]) = reflectEffect(TransformableFind(root, selector))

  override def infix_append(target: Rep[NodeSelection], node: Rep[Node]) = reflectEffect(SelectionAppend(target, node))
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case NodeRefTransform(r, o, n, body) => syms(r):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case NodeRefTransform(r, o, n, body) => o :: n :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case NodeRefTransform(r, o, n, body) => freqNormal(r):::freqHot(body)
    case _ => super.symsFreq(e)
  }

}

trait JSGenTreeManipulation extends JSGenEffect {

  val IR: TreeManipulationExp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {

    case NodeRefTransform(root, o, n, body) => {
      stream.println("var " + quote(n) + " = " + quote(root))
      emitBlock(body)
      emitValDef(sym, quote(getBlockResult(body)))
    }

    case TransformableFind(root, selector) => {
      emitValDef(sym, "$(%s).find(%s)".format(quote(root), quote(selector)))
    }

    case SelectionAppend(target, node) => {
      emitValDef(sym, quote(target) + ".append(" + quote(node) + ")")
    }

    case _ => super.emitNode(sym, node)

  }

}

trait ScalaGenTreeManipulation extends ScalaGenEffect {

  val IR: TreeManipulationExp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {

    case NodeRefTransform(root, o, n, body) => {
      stream.println("val " + quote(o) + " = new org.fusesource.scalate.scuery.Transformer {")
      stream.println("val " + quote(n) + " = " + quote(root) + ".cell")
      emitBlock(body)
      stream.println("}")
      emitValDef(sym, "%s.cell = %s(%s.cell)".format(quote(root), quote(o), quote(root)))
    }

    case TransformableFind(_, selector) => {
      emitValDef(sym, "$(%s)".format(quote(selector)))
    }

    case SelectionAppend(target, node) => {
      emitValDef(sym, quote(target) + "(_ match { case e:xml.Elem => e.copy(child=e.child++" + quote(node) + ")})")
    }

    case _ => super.emitNode(sym, node)

  }

}
