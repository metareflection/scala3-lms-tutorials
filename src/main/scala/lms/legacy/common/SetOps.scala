package scala.lms
package common

import java.io.PrintWriter
import scala.lms.internal._
import scala.collection.mutable.Set

trait SetOps extends Base {
  implicit def setTyp[T:Typ]: Typ[Set[T]]

  object Set {
    def apply[A:Typ](xs: Rep[A]*)(implicit pos: SourceContext) = set_new[A](xs)
  }

  implicit def repSetToSetOps[A:Typ](v: Rep[Set[A]]): setOpsCls[A] = new setOpsCls(v)

  class setOpsCls[A:Typ](s: Rep[Set[A]]) {
    def contains(i: Rep[A])(implicit pos: SourceContext) = set_contains(s, i)
    def add(i: Rep[A])(implicit pos: SourceContext) = set_add(s, i)
    def remove(i: Rep[A])(implicit pos: SourceContext) = set_remove(s, i)
    def size(implicit pos: SourceContext) = set_size(s)
    def clear()(implicit pos: SourceContext) = set_clear(s)
    def toSeq(implicit pos: SourceContext) = set_toseq(s)
    def toArray(implicit pos: SourceContext) = set_toarray(s)
  }

  def set_new[A:Typ](xs: Seq[Rep[A]])(implicit pos: SourceContext) : Rep[Set[A]]
  def set_contains[A:Typ](s: Rep[Set[A]], i: Rep[A])(implicit pos: SourceContext) : Rep[Boolean]
  def set_add[A:Typ](s: Rep[Set[A]], i: Rep[A])(implicit pos: SourceContext) : Rep[Unit]
  def set_remove[A:Typ](s: Rep[Set[A]], i: Rep[A])(implicit pos: SourceContext) : Rep[Unit]
  def set_size[A:Typ](s: Rep[Set[A]])(implicit pos: SourceContext) : Rep[Int]
  def set_clear[A:Typ](s: Rep[Set[A]])(implicit pos: SourceContext) : Rep[Unit]
  def set_toseq[A:Typ](s: Rep[Set[A]])(implicit pos: SourceContext): Rep[Seq[A]]
  def set_toarray[A:Typ](s: Rep[Set[A]])(implicit pos: SourceContext): Rep[Array[A]]
}

trait SetOpsExp extends SetOps with ArrayOps with BooleanOps with EffectExp {
  implicit def setTyp[T:Typ]: Typ[Set[T]] = {
    implicit val ManifestTyp(m: Manifest[T]) = typ[T]
    manifestTyp
  }

  case class SetNew[A:Typ](xs: Seq[Exp[A]], mA: Typ[A]) extends Def[Set[A]]
  case class SetContains[A:Typ](s: Exp[Set[A]], i: Exp[A]) extends Def[Boolean]
  case class SetAdd[A:Typ](s: Exp[Set[A]], i: Exp[A]) extends Def[Unit]
  case class SetRemove[A:Typ](s: Exp[Set[A]], i: Exp[A]) extends Def[Unit]
  case class SetSize[A:Typ](s: Exp[Set[A]]) extends Def[Int]
  case class SetClear[A:Typ](s: Exp[Set[A]]) extends Def[Unit]
  case class SetToSeq[A:Typ](s: Exp[Set[A]]) extends Def[Seq[A]]
  case class SetToArray[A:Typ](s: Exp[Set[A]]) extends Def[Array[A]] {
    //val array = unit(manifest[A].newArray(0))
    val array = NewArray[A](set_size(s))
  }

  def set_new[A:Typ](xs: Seq[Exp[A]])(implicit pos: SourceContext) = reflectMutable(SetNew(xs, typ[A]))
  def set_contains[A:Typ](s: Exp[Set[A]], i: Exp[A])(implicit pos: SourceContext) = SetContains(s, i)
  def set_add[A:Typ](s: Exp[Set[A]], i: Exp[A])(implicit pos: SourceContext) = reflectWrite(s)(SetAdd(s, i))
  def set_remove[A:Typ](s: Exp[Set[A]], i: Exp[A])(implicit pos: SourceContext) = reflectWrite(s)(SetRemove(s, i))
  def set_size[A:Typ](s: Exp[Set[A]])(implicit pos: SourceContext) = SetSize(s)
  def set_clear[A:Typ](s: Exp[Set[A]])(implicit pos: SourceContext) = reflectWrite(s)(SetClear(s))
  def set_toseq[A:Typ](s: Exp[Set[A]])(implicit pos: SourceContext) = SetToSeq(s)
  def set_toarray[A:Typ](s: Exp[Set[A]])(implicit pos: SourceContext) = SetToArray(s)
}

trait BaseGenSetOps extends GenericNestedCodegen {
  val IR: SetOpsExp
  import IR._

}

trait ScalaGenSetOps extends BaseGenSetOps with ScalaGenEffect {
  val IR: SetOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case SetNew(xs, mA) => emitValDef(sym, src"collection.mutable.HashSet[$mA](" + (xs map {quote}).mkString(",") + ")")
    case SetContains(s,i) => emitValDef(sym, src"$s.contains($i)")
    case SetAdd(s,i) => emitValDef(sym, src"$s.add($i)")
    case SetRemove(s,i) => emitValDef(sym, src"$s.remove($i)")
    case SetSize(s) => emitValDef(sym, src"$s.size")
    case SetClear(s) => emitValDef(sym, src"$s.clear()")
    case SetToSeq(s) => emitValDef(sym, src"$s.toSeq")
    case n@SetToArray(s) => //emitValDef(sym, quote(s) + ".toArray")
      gen"""// workaround for refinedTyp problem
           |val $sym = {
           |val out = $n.array
           |val in = $s.toSeq
           |var i = 0
           |while (i < in.length) {
           |out(i) = in(i)
           |i += 1
           |}
           |out
           |}"""
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenSetOps extends BaseGenSetOps with CLikeCodegen {
  val IR: SetOpsExp
  import IR._

//  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
//    case _ => super.emitNode(sym, rhs)
//  }
}

trait CudaGenSetOps extends CudaGenEffect with CLikeGenSetOps
trait OpenCLGenSetOps extends OpenCLGenEffect with CLikeGenSetOps
trait CGenSetOps extends CGenEffect with CLikeGenSetOps
