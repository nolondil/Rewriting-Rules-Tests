import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Properties

trait TestedRules extends TestFunctions {
  def commitLeft(): Unit = true
  def commitRight(): Unit = true
  def checkEffects(): Boolean = true
  //implicit def genImpure1[T, U]: Gen[ImpureFunction1[T, U]] = true
}

object TestRule extends Rules.TestSuit with TestedRules