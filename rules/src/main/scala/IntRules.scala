import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

@rewrites
object IntRules {
  def twoMaps(xs: List[Int], f1: (Int => Int), f2: (Int => Int)) =
    Rewrite(xs.map(f1).map(f2),
      xs.map(x => f2(f1(x))))

  def filterAndMap(xs: List[Int], p: (Int => Boolean), f: (Int => Int)) =
    Rewrite(xs.filter(p).map(f),
            for (x <- xs if p(x)) yield f(x))

  def

  def main(args: Array[String]): Unit = {
    List(1,2,3,4).map(x => 2*x).map(x => x + 4)
    List(1,2,3,4).filter(x => x % 2 == 0).map(x => x + 1)
  }
}
