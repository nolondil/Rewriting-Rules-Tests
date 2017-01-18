import dotty.linker._

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Test



@rewrites
object IntRulesImpure {
  var buff = scala.collection.mutable.ListBuffer.empty[String]

  def emptyBuff: Unit = {
    buff.clear()
  }

  case class Impure(val f: (Int => Int), val name: String) extends (Int => Int) { 
    def apply(i: Int): Int = { buff.append(name); f(i) }
    override def toString: String = "Function " + name
  }

  def genImpureFunctions : Gen[Impure] = for {
    name <- Gen.choose('a', 'z')
    fun  <- arbitrary[(Int => Int)] 
  } yield Impure(fun, name.toString)

  case class ImpureBoolean(val f: (Int => Boolean), val name: String) extends (Int => Boolean) {
    def apply(i: Int): Boolean = { buff.append(name); f(i) }
    override def toString: String = "Boolean Function " + name
  }
  
  def genImpureBoolean : Gen[ImpureBoolean] = for {
    name <- Gen.choose('a', 'z')
    fun <- arbitrary[(Int => Boolean)]
  } yield ImpureBoolean(fun, name.toString)

  case class ImpureOp(val f: (Int, Int) => Int, val name: String) extends ((Int, Int) => Int) {
    def apply(left: Int, right: Int): Int = { buff.append(name); f(left, right) }
    override def toString: String = "Op " + name
  }

  def genImpureOp: Gen[ImpureOp] = for {
    name <- Gen.choose('a', 'z')
    fun <- arbitrary[((Int, Int) => Int)]
  } yield ImpureOp(fun, name.toString)

  implicit lazy val arbImpureFunctions : Arbitrary[Impure] = Arbitrary(genImpureFunctions)

  implicit lazy val arbImpureBooleanFunctions : Arbitrary[ImpureBoolean] = Arbitrary(genImpureBoolean)

  implicit lazy val arbImpureOp: Arbitrary[ImpureOp] = Arbitrary(genImpureOp)

  def twoMapsf1Impure(f1: Impure, f2: Int => Int, xs: Seq[Int]) =
    Rewrite(
      xs.map(f1).map(f2),
      xs.map(x => f2(f1(x)))
    )

  def twoMapsf2Impure(f1: (Int => Int), f2: Impure, xs: Seq[Int]) =
    Rewrite(
      xs.map(f1).map(f2),
      xs.map(x => f2(f1(x)))
    )

  def twoMapsBothImpure(f1: Impure, f2: Impure, xs: Seq[Int]) =
    Rewrite(
      xs.map(f1).map(f2),
      xs.map(x => f2(f1(x)))
    )
}
