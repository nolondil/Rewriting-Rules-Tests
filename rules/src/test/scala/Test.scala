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

object TestVal extends IntRules.ValidityTests

object TestSpeed extends IntRules.EfficiencyTests

object TestEffects extends IntRules.FunctionImpurityTests
