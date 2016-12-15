/*import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
*/
/*object Rewrite {
  def apply(from: Any, to: Any): Unit = ???
}

trait IsPure[T]

//@rewrites
object Rules {
  def isEmpty(x: Seq[Int]) =
    Rewrite(x.length == 0,
            x.isEmpty)

  def twoDropRights(x: List[Int], a: Int, b: Int) =
    Rewrite(x.dropRight(a).dropRight(b),
            {
              val a1 = Math.max(0, a)
              val a2 = Math.max(0, b)
              if (a1/2 + a2/2 >= Int.MaxValue/2) Nil
              else x.dropRight(a1 + a2)
            })

  def twoFilters(x: List[Int], a: Int => Boolean, b: Int => Boolean)(implicit apure: IsPure[a.type]) =
    Rewrite(x.filter(a).filter(b),
            x.filter(x => a(x) && b(x)))
}*/
