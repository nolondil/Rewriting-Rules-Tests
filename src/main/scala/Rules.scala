import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

object Rewrite {
  def apply(from: Any, to: Any): Unit = ???
}

trait IsPure[T]

class TestedRules(name: String) extends Properties(name: String) {
  def commitLeft(): Unit = true
  def commitRight(): Unit = true
  def checkEffects(): Boolean = true
  //implicit def genImpure1[T, U]: Gen[ImpureFunction1[T, U]] = true
}

/*package scalacheck {
  trait Gen[T]

  trait ImpureFunction1[-T, +U] extends Function1[T, U]

  object property {
    def update(name: String, gen: Any): Unit = ???
  }

  object forAll {
    def apply(gen: Any): Any = ???
  }
}*/

@rewrites
object Rules {
  def isEmpty(x: Seq[Int]) =
    Rewrite(x.length == 0,
            x.isEmpty)

  def twoDropRights(x: List[Int], a: Int, b: Int) =
    Rewrite(x.dropRight(a).dropRight(b),
            x.dropRight(a + b))

  /*def twoFilters(x: List[Int], a: Int => Boolean, b: Int => Boolean)(implicit apure: IsPure[a.type]) =
    Rewrite(x.filter(a).filter(b),
            x.filter(x => a(x) && b(x)))*/
}

// object Rules extends TestedRules("Rules") {
//   def isEmpty(x: Seq[Int]) =
//     Rewrite(from = x.length == 0,
//             to   = x.isEmpty)
//
//   property("isEmpty") = forAll {
//       (x: Seq[Int]) => {
//         val left = (x.length == 0)
//         commit$Left()
//         var right = x.isEmpty
//         commit$Right()
//         (left == right) && checkEffects
//       }
//   }
//
//   def twoDropRights(x: List[Int], a: Int, b: Int) =
//     Rewrite(from = x.dropRight(a).dropRight(b),
//             to   = x.dropRight(a + b))
//
//   property("twoDropRights") = forAll {
//       (x: List[Int], a: Int, b: Int) => {
//        val left = x.dropRight(a).dropRight(b)
//        commit$Left()
//        val right = x.dropRight(a + b)
//        commit$Right()
//        (left == right) && checkEffects
//       }
//   }
//
//   def twoFilters(x: List[Int], a: Int => Boolean, b: Int => Boolean)(implicit apure: IsPure[a.type]) =
//     Rewrite(from = x.filter(a).filter(b),
//             to   = x.filter(x => a(x) && b(x)))
//
//   property("twoFilters") = forAll {
//       (x: List[Int], a: Int => Boolean, b: ImpureFunction1[Int, Boolean]) => {
//         val left = x.filter(a).filter(b)
//         commit$Left()
//         val right = x.filter(x => a(x) && b(x))
//         commit$Right()
//         (left == right) && checkEffects
//       }
//   }
// }
