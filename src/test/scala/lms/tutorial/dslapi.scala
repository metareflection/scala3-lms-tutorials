package scala.lms.tutorial

import java.io._

import lms._
import lms.common._

// should this be added to LMS?
trait UtilOps extends Base { this: Dsl =>
  def infix_HashCode[T:Typ](o: Rep[T])(implicit pos: SourceContext): Rep[Long]
  def infix_HashCode(o: Rep[String], len: Rep[Int])(implicit v: Overloaded1, pos: SourceContext): Rep[Long]
}
trait UtilOpsExp extends UtilOps with BaseExp { this: DslExp =>
  case class ObjHashCode[T:Typ](o: Rep[T])(implicit pos: SourceContext) extends Def[Long] { def m = typ[T] }
  case class StrSubHashCode(o: Rep[String], len: Rep[Int])(implicit pos: SourceContext) extends Def[Long]
  def infix_HashCode[T:Typ](o: Rep[T])(implicit pos: SourceContext) = ObjHashCode(o)
  def infix_HashCode(o: Rep[String], len: Rep[Int])(implicit v: Overloaded1, pos: SourceContext) = StrSubHashCode(o,len)

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@ObjHashCode(a) => infix_HashCode(f(a))(using e.m,pos)
    case e@StrSubHashCode(o,len) => infix_HashCode(f(o),f(len))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}
trait ScalaGenUtilOps extends ScalaGenBase {
  val IR: UtilOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjHashCode(o) => emitValDef(sym, src"$o.##")
    case _ => super.emitNode(sym, rhs)
  }
}

trait Dsl extends PrimitiveOps with NumericOps with BooleanOps with LiftString with LiftPrimitives with LiftNumeric with LiftBoolean with IfThenElse with Equal with RangeOps with OrderingOps with MiscOps with ArrayOps with StringOps with SeqOps with Functions with While with StaticData with Variables with LiftVariables with ObjectOps with UtilOps {
  implicit def repStrToSeqOps(a: Rep[String]): SeqOpsCls[Char] = new SeqOpsCls(a.asInstanceOf[Rep[Seq[Char]]])
  override def infix_&&(lhs: Rep[Boolean], rhs: => Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean] =
    __ifThenElse(lhs, rhs, unit(false))
  override def infix_||(lhs: Rep[Boolean], rhs: => Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean] =
    __ifThenElse(lhs, unit(true), rhs)
  def generate_comment(l: String): Rep[Unit]
  def comment[A:Typ](l: String, verbose: Boolean = true)(b: => Rep[A]): Rep[A]
}

trait DslExp extends Dsl with PrimitiveOpsExpOpt with NumericOpsExpOpt with BooleanOpsExp with IfThenElseExpOpt with EqualExpBridgeOpt with RangeOpsExp with OrderingOpsExp with MiscOpsExp with EffectExp with ArrayOpsExpOpt with StringOpsExp with SeqOpsExp with FunctionsRecursiveExp with WhileExp with StaticDataExp with VariablesExpOpt with ObjectOpsExpOpt with UtilOpsExp {
  override def boolean_or(lhs: Exp[Boolean], rhs: Exp[Boolean])(implicit pos: SourceContext) : Exp[Boolean] = lhs match {
    case Const(false) => rhs
    case _ => super.boolean_or(lhs, rhs)
  }
  override def boolean_and(lhs: Exp[Boolean], rhs: Exp[Boolean])(implicit pos: SourceContext) : Exp[Boolean] = lhs match {
    case Const(true) => rhs
    case _ => super.boolean_and(lhs, rhs)
  }

  case class GenerateComment(l: String) extends Def[Unit]
  def generate_comment(l: String) = reflectEffect(GenerateComment(l))
  case class Comment[A:Typ](l: String, verbose: Boolean, b: Block[A]) extends Def[A]
  def comment[A:Typ](l: String, verbose: Boolean)(b: => Rep[A]): Rep[A] = {
    val br = reifyEffects(b)
    val be = summarizeEffects(br)
    reflectEffect[A](Comment(l, verbose, br), be)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Comment(_, _, b) => effectSyms(b)
    case _ => super.boundSyms(e)
  }

  override def array_apply[T:Typ](x: Exp[Array[T]], n: Exp[Int])(implicit pos: SourceContext): Exp[T] = (x,n) match {
    case (Def(StaticData(x:Array[T])), Const(n)) =>
      val y = x(n)
      if (y.isInstanceOf[Int]) unit(y) else staticData(y)
    case _ => super.array_apply(x,n)
  }

  // TODO: should this be in LMS?
  override def isPrimitiveType[T](m: Typ[T]) = (m == manifest[String]) || super.isPrimitiveType(m)
}
trait DslGen extends ScalaGenNumericOps
    with ScalaGenPrimitiveOps with ScalaGenBooleanOps with ScalaGenIfThenElse
    with ScalaGenEqual with ScalaGenRangeOps with ScalaGenOrderingOps
    with ScalaGenMiscOps with ScalaGenArrayOps with ScalaGenStringOps
    with ScalaGenSeqOps with ScalaGenFunctions with ScalaGenWhile
    with ScalaGenStaticData with ScalaGenVariables
    with ScalaGenObjectOps
    with ScalaGenUtilOps {
  val IR: DslExp

  import IR._

  override def quote(x: Exp[Any]) = x match {
    case Const('\n') if x.tp == typ[Char] => "'\\n'"
    case Const('\t') if x.tp == typ[Char] => "'\\t'"
    case Const(0)    if x.tp == typ[Char] => "'\\0'"
    case _ => super.quote(x)
  }
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case IfThenElse(c,Block(Const(true)),Block(Const(false))) =>
      emitValDef(sym, quote(c))
    case PrintF(f:String,xs) => 
      emitValDef(sym, src"printf(${Const(f)::xs})")
    case GenerateComment(s) =>
      stream.println("// "+s)
    case Comment(s, verbose, b) =>
      stream.println("val " + quote(sym) + " = {")
      stream.println("//#" + s)
      if (verbose) {
        stream.println("// generated code for " + s.replace('_', ' '))
      } else {
        stream.println("// generated code")
      }
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("//#" + s)
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}
trait DslImpl extends DslExp { q =>
  val codegen = new DslGen {
    val IR: q.type = q
  }
}

abstract class DslSnippet[A:Manifest,B:Manifest] extends Dsl {
  def snippet(x: Rep[A]): Rep[B]
}

abstract class DslDriver[A,B](using mA: Manifest[A], mB: Manifest[B]) extends DslSnippet[A,B] with DslImpl {
  lazy val code: String = {
    val source = new java.io.StringWriter()
    codegen.emitSource(snippet, "Snippet", new java.io.PrintWriter(source))
      (using ManifestTyp(mA), ManifestTyp(mB))
    source.toString
  }
}
