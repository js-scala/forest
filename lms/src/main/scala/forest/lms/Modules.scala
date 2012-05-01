package forest.lms

import virtualization.lms.common._
import js._

trait Modules { self: Base with JSProxyBase =>

  /** A Module is a singleton implementing a given type A */
  abstract class Module[A]
  object Module {
    implicit def toA[A <: AnyRef : Manifest](m: Rep[Module[A]]): A = moduleToA(m)
  }

  /**
   * Returns the module of a given trait (which must not be abstract).
   * The module is a singleton with type A.
   * {{{
   *   trait Foo {
   *     def foo(x: Rep[Int]) = x + 1
   *   }
   *   val Foo = module[Foo]
   *   Foo.foo(42)
   * }}}
   */
  def module[A <: AnyRef : Manifest]: Rep[Module[A]]

  protected def moduleToA[A <: AnyRef : Manifest](m: Rep[Module[A]]): A

  implicit val __base: Base = self
}

trait ModulesExp extends BaseExp with Modules { this: EffectExp with JSProxyExp =>

  /** Module definition */
  case class ModuleDef[A : Manifest](methods: List[MethodDef]) extends Def[Module[A]]
  case class MethodDef(name: String, params: List[(Sym[Any], String)], body: Exp[Any])
  case class Self[A : Manifest]() extends Exp[A]
  case class Get[A : Manifest](module: Exp[Module[A]]) extends Exp[A]

  // TODO Use the new reflection API
  override def module[A <: AnyRef : Manifest]: Exp[Module[A]] = {
    // Reminder: A Scala trait T is compiled in a Java interface T + a Java class T$class
    val interfaceClazz = manifest[A].erasure
    val classClazz = Class.forName(interfaceClazz.getName + "$class")

    assert(classClazz != null, "Unable to find implementation for trait " + interfaceClazz.getName)
    assert(interfaceClazz.getInterfaces.length <= 1, "Inheritance is not supported")

    val methods = for (method <- classClazz.getDeclaredMethods if method.getName != "$init$") yield {
      val params = for (p <- method.getParameterTypes.drop(1).toList) yield (fresh[Any], p.getName) // FIXME I should use something else than the class name (a manifest?)
      val self = proxyTrait[A](Self[A](), None) // TODO handle inheritance
      val args = (self :: params.map(_._1)).toArray
      MethodDef(method.getName, params, reifyEffects(method.invoke(null, args: _*).asInstanceOf[Exp[Any]]))
    }

    ModuleDef[A](methods.toList)
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case MethodDef(_, _, body) => syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case MethodDef(_, params, _) => params.map(_._1).flatMap(syms)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case MethodDef(_, _, body) => freqHot(body)
    case _ => super.symsFreq(e)
  }

  override protected def moduleToA[A <: AnyRef : Manifest](m: Exp[Module[A]]): A = repProxy(Get(m))

  // Syntactic sugar on top of the JSProxy API
  def proxyTrait[T <: AnyRef](x: Rep[T], parentCtor: Option[Rep[Any]])(implicit outer: Base, m: Manifest[T]): T = proxyTrait(x, parentCtor, outer)
}

import js._
import java.io.PrintWriter

trait JSGenModules extends JSGenBase {
  val IR: ModulesExp with BaseExp with JSLiteralExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {

    case ModuleDef(methods) => {
      stream.println("var " + quote(sym) + " = {")
      for ((method, i) <- methods.zipWithIndex) {
        val params = method.params.map( p => quote(p._1) ).mkString(",")
        stream.println("'%s': function (%s) {".format(method.name, params))
        emitBlock(method.body)
        stream.println("return %s;".format(quote(getBlockResult(method.body))))
        if (i < methods.length - 1)
          stream.println("},")
        else
          stream.println("}")
      }
      stream.println("};")
    }

    case _ => super.emitNode(sym, rhs)

  }

  override def quote(x: Exp[Any]): String = x match {
    case Self() => "this" // TODO something more robust
    case Get(module) => quote(module)
    case _ => super.quote(x)
  }

}

trait ScalaGenModules