package forest

import java.io.PrintWriter
import scala.js.language.Proxy
import scala.js.exp.ProxyExp
import scala.js.gen.js.GenEffect
import scala.js.{JSInScala, JSProxyInScala}
import scala.virtualization.lms.common.{Base, EffectExp, ScalaGenEffect}

trait Modules extends Base with Proxy { self =>
  import language.implicitConversions

  /** A Module is a singleton implementing a given type A */
  sealed abstract class Module[A]

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

  protected implicit def moduleToA[A <: AnyRef : Manifest](m: Rep[Module[A]]): A

  implicit val __base: Base = self

}

trait ModulesExp extends Modules with EffectExp with ProxyExp {
  import language.implicitConversions

  /** Module definition */
  case class ModuleDef[A : Manifest](methods: List[MethodDef]) extends Def[Module[A]]
  case class MethodDef(name: String, params: List[(Sym[Any], String)], body: Block[Any])
  case class Self[A : Manifest]() extends Exp[A] {
    private var _self: Exp[Module[A]] = _
    def self = {
      if (_self == null) sys.error("Oops. Self has not been set up!")
      else _self
    }
    def self_=(a: Exp[Module[A]]) { _self = a }
  }
  case class Get[A : Manifest](module: Exp[Module[A]]) extends Exp[A] // TODO Unify Get and Self

  // TODO Use the new reflection API
  override def module[A <: AnyRef : Manifest]: Exp[Module[A]] = {
    // Reminder: A Scala trait T is compiled in a Java interface T + a Java class T$class
    val interfaceClazz = manifest[A].runtimeClass
    val classClazz = Class.forName(interfaceClazz.getName + "$class")

    assert(classClazz != null, "Unable to find implementation for trait " + interfaceClazz.getName)
    assert(interfaceClazz.getInterfaces.length <= 1, "Inheritance is not supported")

    val selfExp = Self[A]()
    val self = proxyTrait[A](selfExp, None) // TODO handle inheritance
    val methods = for (method <- classClazz.getDeclaredMethods if method.getName != "$init$") yield { // FIXME Generate initialization code?
      val params = for (p <- method.getParameterTypes.drop(1).toList) yield (fresh[Any], p.getName) // FIXME I should use something else than the class name (a manifest?)
      val args = (self :: params.map(_._1)).toArray
      MethodDef(method.getName, params, reifyEffects(method.invoke(null, args: _*).asInstanceOf[Exp[Any]]))
    }

    val m = ModuleDef[A](methods.toList)
    selfExp.self = m
    m
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case MethodDef(_, _, body) => syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case MethodDef(_, params, body) => params.map(_._1).flatMap(syms) ::: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case MethodDef(_, _, body) => freqHot(body)
    case _ => super.symsFreq(e)
  }

  override protected implicit def moduleToA[A <: AnyRef : Manifest](m: Exp[Module[A]]): A = repProxy(Get(m))

  // Syntactic sugar on top of the JSProxy API
  def proxyTrait[T <: AnyRef](x: Rep[T], parentCtor: Option[Rep[Any]])(implicit outer: Base, m: Manifest[T]): T = proxyTrait(x, parentCtor, outer)
}

trait JSGenModules extends scala.js.gen.js.GenEffect {
  val IR: ModulesExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {

    case ModuleDef(methods) => {
      stream.println("var " + quote(sym) + " = {")
      stream.println(
        (for (method <- methods) yield {
          "'" + method.name + "': " + quoteMethod(method)
        }).mkString(",\n")
      )
      stream.println("};")
    }

    case _ => super.emitNode(sym, rhs)

  }

  override def quote(x: Exp[Any]): String = x match {
    case s @ Self() => quote(s.self)
    case Get(module) => quote(module)
    case _ => super.quote(x)
  }

  def emitModule[A : Manifest](module: Exp[Module[A]], name: String, stream: PrintWriter) = module match {
    case Def(ModuleDef(methods)) => {
      stream.println("window." + name + " = (function (" + quote(module) + ") {")
      for (method <- methods) {
        stream.println(quote(module) + "['" + method.name + "'] = " + quoteMethod(method) + ";")
      }
      stream.println("return " + quote(module))
      stream.println("})(window." + name + " || {});")
      stream.flush()
    }
    case _ => sys.error(s"What kind of module are you? ($module)")
  }

  // That’s not pretty. But more reusable.
  def quoteMethod(method: MethodDef): String = {
    val builder = new StringBuilder
    val params = method.params.map( p => quote(p._1) ).mkString(",")
    builder ++= "function (%s) {\n".format(params)
    val bodyWriter = new java.io.StringWriter()
    withStream(new java.io.PrintWriter(bodyWriter)) {
      emitBlock(method.body)
    }
    builder ++= bodyWriter.toString()
    builder ++= "return %s\n".format(quote(getBlockResult(method.body)))
    builder ++= "}"
    builder.result()
  }

}

trait ModulesInScala extends Modules with JSInScala with JSProxyInScala {
  import language.implicitConversions

  case class ModuleW[A](a: A) extends Module[A]

  override def module[A <: AnyRef : Manifest]: Module[A] =  new ModuleW(create[A])

  def create[A : Manifest]: A = sys.error("Unable to create a value of type %s".format(manifest[A].runtimeClass.getName))

  override protected implicit def moduleToA[A <: AnyRef : Manifest](m: Module[A]): A = m match {
    case ModuleW(a) => a
  }

}

trait ScalaGenModules extends ScalaGenEffect {
  val IR: ModulesExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {

    case ModuleDef(methods) => {
      stream.println("object " + quote(sym) + " {")
      for (method <- methods) {
        val params = method.params.map { p => "%s: %s".format(quote(p._1), p._2) }.mkString("(", ", ", ")")
        stream.println("def %s%s = {".format(method.name, params)) // TODO Omit “=” for methods returning Unit, add parens for side effecting methods with arity zero
        emitBlock(method.body)
        stream.println("}")
      }
      stream.println("")
      stream.println("}")
    }

    case _ => super.emitNode(sym, rhs)

  }

  override def quote(x: Exp[Any]): String = x match {
    case s @ Self() => quote(s.self)
    case Get(module) => quote(module)
    case _ => super.quote(x)
  }

}
