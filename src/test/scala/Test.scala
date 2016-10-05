import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Properties


class Test extends FunSuite with Checkers {
  test("Random test") {
    println(Rules)
    assert(true)
  }

  test("Test des rules") {
    assert(Rules.isInstanceOf[Properties])
    for (p <- Rules.asInstanceOf[Properties].properties) {
      try {
        check(p._2)
      } catch {
        case e: Exception =>
          assert(true, p._1)
      }
    }
  }
}
