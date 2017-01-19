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


object TestCorrectness extends Rules.Correctness

object TestEfficiency extends Rules.Efficiency

object TestEffects extends Rules.FunctionSideEffects

object TestEffectsBad extends BadExamples.FunctionSideEffects

class TestImpure extends FunSuite with Checkers {
  object TestGeneralImpurity extends IntRulesImpure.GeneralSideEffects {
    var leftValue: List[String] = Nil
    var rightValue: List[String] = Nil
    def cleanUp() = IntRulesImpure.emptyBuff
    def commitLeft() = {
      leftValue = IntRulesImpure.buff.toList
      IntRulesImpure.emptyBuff
    }
    def commitRight() = {
      rightValue = IntRulesImpure.buff.toList
      IntRulesImpure.emptyBuff
    }
    def checkEffects() = leftValue =? rightValue
  }

  test("Self designed side effects") {
    for (p <- TestGeneralImpurity.asInstanceOf[Properties].properties) {
      check(p._2)
    }
  }
}
