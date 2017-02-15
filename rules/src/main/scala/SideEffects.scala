import dotty.linker._

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import org.scalacheck.Gen._
import org.scalacheck.Test

@rewrites
object BadExamples {
  class MyInt(val i: Int) {
    def toLong: Long = i.toLong  
  }

  val genMyInt = for (a <-arbitrary[Int]) yield new MyInt(a)
  implicit lazy val arbMyInt = Arbitrary(genMyInt)
  implicit lazy val cogenMyInt : Cogen[MyInt] = Cogen(_.toLong)
  /*val genFun = for (fun <- arbitrary[Int => Int]) yield { x: MyInt => fun(x._i) }
  implicit lazy val arbMyIntInt = Arbitrary(genFun)*/

  def needString(f: (MyInt => Int)) =
    Rewrite(
    f(new MyInt(5)),
    f(new MyInt(5))
  )
}
