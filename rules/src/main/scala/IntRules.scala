import dotty.linker._

@rewrites
object IntRules {
  /*def twoMaps(xs: Seq[Int], f1: (Int => Int), f2: (Int => Int)) =
    Rewrite(xs.map(f1).map(f2),
      xs.map(x => f2(f1(x))))

  def filterAndMap(xs: Seq[Int], p: (Int => Boolean), f: (Int => Int)) =
    Rewrite(
      xs.filter(p).map(f),
      for (x <- xs if p(x)) yield f(x) // Need to be rewritten with collect ? (some troobleshooting with macros)
    )

  def mapAndFilter(xs: Seq[Int], f: (Int => Int), p: (Int => Boolean)) =
    Rewrite(
      xs.map(f).filter(p),
      xs.collect(f(_) match { case y if p(y) => y })
    )*/

  def isEmpty(x: Seq[Int]) =
    Rewrite(
      x.length == 0,
      x.isEmpty
    )

  def twoDropRights(x: Seq[Int], a: Int, b: Int) =
    Rewrite(
      x.dropRight(a).dropRight(b),
      {
        val a1 = Math.max(0, a)
        val a2 = Math.max(0, b)
        if (a1/2 + a2/2 >= Int.MaxValue/2) Nil
        else x.dropRight(a1 + a2)
      }
    )

  private def sliceFromDrops(xs: Seq[Int], r: Int, l: Int) = {
    val size = xs.length
    val first = Math.max(0, l)
    val r1 = Math.max(0, r)
    val last = size - r1
    xs.slice(first, last)
  }

  def dropRightAndLeft(xs: Seq[Int], r: Int, l: Int) =
    Rewrite(
      xs.dropRight(r).drop(l),
      sliceFromDrops(xs, r, l)
    )

  def dropAndDropRight(xs: Seq[Int], r: Int, l: Int) =
    Rewrite(
      xs.drop(l).dropRight(r),
      sliceFromDrops(xs, r, l)
    )

  def takeAndDropRight(xs: Seq[Int], n: Int, d: Int) =
    Rewrite(
      xs.take(n).dropRight(d),
      {
        val n1 = Math.max(0, n)
        val n2 = Math.min(n1, xs.length)
        val d1 = Math.max(0, d)
        if (d1 > n2) Nil
        else xs.slice(0, n2 - d1)
      }
    )

  def main(args: Array[String]): Unit = {
    val sequence : Seq[Int] = (1 to 10)
    sequence.isEmpty
    sequence.dropRight(2).dropRight(3)
    sequence.take(2).dropRight(4)
    //List()
    /*List(1,2,3,4).map(x => 2*x).map(x => x + 4)
    List(1,2,3,4).filter(x => x % 2 == 0).map(x => x + 1)
    List(1,2,3,4).dropRight(1).dropRight(1)*/
  }
}