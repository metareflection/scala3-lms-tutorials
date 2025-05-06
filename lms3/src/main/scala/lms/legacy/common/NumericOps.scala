package scala.lms
package common

import java.io.PrintWriter

trait LiftNumeric {
  this: Base =>

  implicit def numericToNumericRep[T:Numeric:Typ](x: T): Rep[T] = unit(x)
}

trait NumericOps extends Variables {
  this: PrimitiveOps =>

  // workaround for infix not working with manifests
  implicit def numericToNumericOps[T:Numeric:Typ](n: T): NumericOpsCls[T]  = new NumericOpsCls(unit(n))
  implicit def repNumericToNumericOps[T:Numeric:Typ](n: Rep[T]): NumericOpsCls[T] = new NumericOpsCls(n)
  implicit def varNumericToNumericOps[T:Numeric:Typ](n: Var[T]): NumericOpsCls[T] = new NumericOpsCls(readVar(n))
  
  class NumericOpsCls[T:Numeric:Typ](lhs: Rep[T]){
    def +[A](rhs: A)(implicit c: A => T, pos: SourceContext) = numeric_plus(lhs,unit(c(rhs)))
    def +(rhs: Rep[T])(implicit pos: SourceContext) = numeric_plus  (lhs,rhs)
    def -(rhs: Rep[T])(implicit pos: SourceContext) = numeric_minus (lhs,rhs)
    def *(rhs: Rep[T])(implicit pos: SourceContext) = numeric_times (lhs,rhs)
    def /(rhs: Rep[T])(implicit pos: SourceContext) = numeric_divide(lhs,rhs)
  }

  def numeric_plus  [T:Numeric:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_minus [T:Numeric:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_times [T:Numeric:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def numeric_divide[T:Numeric:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
}

trait NumericOpsExp extends NumericOps with VariablesExp with BaseFatExp {
  this: PrimitiveOpsExp =>
  
  abstract class NumericDefMN[A:Typ:Numeric] extends Def[A] {
    def mev = typ[A]
    def aev = implicitly[Numeric[A]]
  }

  case class NumericPlus   [T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T]) extends NumericDefMN[T]
  case class NumericMinus  [T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T]) extends NumericDefMN[T]
  case class NumericTimes  [T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T]) extends NumericDefMN[T]
  case class NumericDivide [T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T]) extends NumericDefMN[T]

  def numeric_plus   [T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericPlus(lhs, rhs)
  def numeric_minus  [T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericMinus(lhs, rhs)
  def numeric_times  [T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericTimes(lhs, rhs)
  def numeric_divide [T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericDivide(lhs, rhs)
  
  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@NumericPlus  (l,r) => numeric_plus  (f(l), f(r))(using e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case e@NumericMinus (l,r) => numeric_minus (f(l), f(r))(using e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case e@NumericTimes (l,r) => numeric_times (f(l), f(r))(using e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case e@NumericDivide(l,r) => numeric_divide(f(l), f(r))(using e.aev.asInstanceOf[Numeric[A]], mtype(e.mev), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait NumericOpsExpOpt extends NumericOpsExp {
  this: PrimitiveOpsExp =>
  
  override def numeric_plus[T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = {
    val num = implicitly[Numeric[T]]
    (lhs,rhs) match {
      case (Const(x), Const(y)) => Const(num.plus(x,y))
      case (Const(x), y) if x == num.zero => y
      case (x, Const(y)) if y == num.zero => x
      case _ => super.numeric_plus(lhs,rhs)
    }
  }
  override def numeric_minus[T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
    case (Const(x), Const(y)) => Const(implicitly[Numeric[T]].minus(x,y))
    case (x, Const(y)) if y == implicitly[Numeric[T]].zero => x
    case _ => super.numeric_minus(lhs,rhs)
  }
  override def numeric_times[T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = {
    val num = implicitly[Numeric[T]]
    (lhs,rhs) match {
      case (Const(x), Const(y)) => Const(num.times(x,y))
      case (Const(x), y) if x == num.zero => Const(x)
      case (x, Const(y)) if y == num.zero => Const(y)
      case (Const(x), y) if x == num.one => y
      case (x, Const(y)) if y == num.one => x
      case _ => super.numeric_times(lhs,rhs)
    }
  }
  override def numeric_divide[T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = {
    val num = implicitly[Numeric[T]]
    (lhs,rhs) match {
      // Avoid exception if y == n.zero, this may still not be ultimately used
      case (Const(x), Const(y)) if y != num.zero =>
        num match {
          case f: Fractional[T @unchecked] => Const(f.div(x, y))
          case i: Integral[T @unchecked] => Const(i.quot(x, y))
        }
      case (Const(x), y) if x == implicitly[Numeric[T]].zero => Const(x)
      case (x, Const(y)) if y == implicitly[Numeric[T]].one => x
      case _ => super.numeric_divide(lhs,rhs)
    }
  }
}


trait ScalaGenNumericOps extends ScalaGenFat {
  val IR: NumericOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NumericPlus   (a,b) => emitValDef(sym, src"$a + $b")
    case NumericMinus  (a,b) => emitValDef(sym, src"$a - $b")
    case NumericTimes  (a,b) => emitValDef(sym, src"$a * $b")
    case NumericDivide (a,b) => emitValDef(sym, src"$a / $b")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenNumericOps extends CLikeGenBase {
  val IR: NumericOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
      rhs match {
        case NumericPlus  (a,b) => emitValDef(sym, src"$a + $b")
        case NumericMinus (a,b) => emitValDef(sym, src"$a - $b")
        case NumericTimes (a,b) => emitValDef(sym, src"$a * $b")
        case NumericDivide(a,b) => emitValDef(sym, src"$a / $b")
        case _ => super.emitNode(sym, rhs)
      }
    }
}

trait CudaGenNumericOps extends CudaGenBase with CLikeGenNumericOps
trait OpenCLGenNumericOps extends OpenCLGenBase with CLikeGenNumericOps
trait CGenNumericOps extends CGenBase with CLikeGenNumericOps

