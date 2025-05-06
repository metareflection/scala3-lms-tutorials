package scala.lms
package common

import java.io.PrintWriter
import scala.lms.util.OverloadHack
import scala.lms.internal.{GenerationFailedException}

trait ObjectOps extends Variables with StringOps with OverloadHack {
  def infix_toString(lhs: Rep[Any])(implicit pos: SourceContext) = object_tostring(lhs)
  def infix_ToString(lhs: Rep[Any])(implicit pos: SourceContext) = object_tostring(lhs)
  def infix_unsafeImmutable[A:Typ](lhs: Rep[A])(implicit pos: SourceContext) = object_unsafe_immutable(lhs)
  def infix_unsafeMutable[A:Typ](lhs: Rep[A])(implicit pos: SourceContext) = object_unsafe_mutable(lhs)

  def object_tostring(lhs: Rep[Any])(implicit pos: SourceContext): Rep[String]
  def object_unsafe_immutable[A:Typ](lhs: Rep[A])(implicit pos: SourceContext): Rep[A]
  def object_unsafe_mutable[A:Typ](lhs: Rep[A])(implicit pos: SourceContext): Rep[A]
}

trait ObjectOpsExp extends ObjectOps with StringOpsExp with VariablesExp {
  case class ObjectToString(o: Exp[Any]) extends Def[String]
  case class ObjectUnsafeImmutable[A:Typ](o: Exp[A]) extends Def[A] {
    def m = typ[A]
  }
  case class ObjectUnsafeMutable[A:Typ](o: Exp[A]) extends Def[A] {
    def m = typ[A]
  }

  def object_tostring(lhs: Exp[Any])(implicit pos: SourceContext) = ObjectToString(lhs)
  def object_unsafe_immutable[A:Typ](lhs: Exp[A])(implicit pos: SourceContext) = lhs match {
    // INVESTIGATE: there was an issue where Const(0).unsafeImmutable == Const(0.0). How is this possible? CSE with primitive widening?
    case c@Const(x) => c
    case _ => ObjectUnsafeImmutable(lhs)
  }
  def object_unsafe_mutable[A:Typ](lhs: Exp[A])(implicit pos: SourceContext) = reflectMutable(ObjectUnsafeMutable(lhs))

  //////////////
  // mirroring

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@ObjectUnsafeImmutable(a) => object_unsafe_immutable(f(a))(using mtype(e.m),pos)
    case e@ObjectToString(a) => object_tostring(f(a))
    case Reflect(e@ObjectUnsafeImmutable(a), u, es) => reflectMirrored(Reflect(ObjectUnsafeImmutable(f(a))(using mtype(e.m)), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case Reflect(e@ObjectUnsafeMutable(a), u, es) => reflectMirrored(Reflect(ObjectUnsafeMutable(f(a))(using mtype(e.m)), mapOver(f,u), f(es)))(using mtyp1[A], pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  /////////////////////
  // aliases and sharing

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case ObjectUnsafeImmutable(a) => Nil
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case ObjectUnsafeImmutable(a) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case ObjectUnsafeImmutable(a) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case ObjectUnsafeImmutable(a) => syms(a)
    case _ => super.copySyms(e)
  }
}

trait ObjectOpsExpOpt extends ObjectOpsExp {
  override def object_tostring(lhs: Exp[Any])(implicit pos: SourceContext) = {
    if (lhs.tp.runtimeClass == classOf[String]) lhs.asInstanceOf[Exp[String]]
    else super.object_tostring(lhs)
  }
}

trait ScalaGenObjectOps extends ScalaGenBase {
  val IR: ObjectOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjectToString(lhs) => emitValDef(sym, src"($lhs).toString()")
    case ObjectUnsafeImmutable(x) => emitValDef(sym, src"$x// unsafe immutable")
    case ObjectUnsafeMutable(x) => emitValDef(sym, src"$x// unsafe mutable")
    case _ => super.emitNode(sym, rhs)
  }
}

trait GPUGenObjectOps extends GPUGenBase {
  val IR: ObjectOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjectUnsafeImmutable(x) => 
      emitValDef(sym, quote(x) + "; // unsafe immutable")
      emitPtrDef(sym, x)
    case ObjectUnsafeMutable(x) => 
      emitValDef(sym, quote(x) + "; // unsafe mutable")
      emitPtrDef(sym, x)
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenObjectOps extends CLikeGenBase {
  val IR: ObjectOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjectToString(x) => emitValDef(sym, src"($x).toString()")
    case ObjectUnsafeImmutable(x) => emitValDef(sym, src"$x; // unsafe immutable")
    case ObjectUnsafeMutable(x) => emitValDef(sym, src"$x; // unsafe mutable")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenObjectOps extends CudaGenBase with CLikeGenObjectOps
trait OpenCLGenObjectOps extends OpenCLGenBase with CLikeGenObjectOps
trait CGenObjectOps extends CGenBase with CLikeGenObjectOps 
