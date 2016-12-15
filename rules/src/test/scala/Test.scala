import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalacheck.Gen
import org.scalacheck.Test

trait TestedRules extends TestFunctions {
  def cleanUp(): Unit = {}
  def commitLeft(): Unit = {}
  def commitRight(): Unit = {}
  def checkEffects(): Boolean = true
}

object TestRuleInt extends IntRules.TestSuit with TestedRules

trait TestImpure extends TestFunctions {
  var currentLeft: Seq[String] = Nil
  var currentRight: Seq[String] = Nil
  def cleanUp() = {
    IntRulesImpure.emptyBuff
  }
  def commitLeft() = {
    currentLeft = IntRulesImpure.buff.toSeq
    IntRulesImpure.emptyBuff
  }
  def commitRight() = {
    currentRight = IntRulesImpure.buff.toSeq
    IntRulesImpure.emptyBuff  
  }
  def checkEffects() = {
    if (currentLeft != currentRight) {
      println("left: " + currentLeft)
      println("right: " + currentRight)
    }
    //(currentLeft.isEmpty && currentRight.isEmpty) ||
    currentLeft == currentRight
    //(currentLeft.length == 2 && currentLeft == currentRight)
  }
}


class TestRules extends FunSuite with Checkers {
  object TestIntRulesImpure extends IntRulesImpure.TestSuit with TestImpure  
  test("Random test") {
    assert(true)
  }

  test("Test des rules") {
    assert(TestIntRulesImpure.isInstanceOf[Properties])
    for (p <- TestIntRulesImpure.asInstanceOf[Properties].properties) {
      //try {
        check(p._2)
      /*} catch {
        case _ : Throwable =>
          assert(false, p._1)
      }*/
    }
  }
}
