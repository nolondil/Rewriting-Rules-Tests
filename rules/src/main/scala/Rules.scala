import dotty.linker._

@rewrites
object Rules {
  def wrongToMap(xs: Seq[Int], f1: (Int => Int), f2: (Int => Int)) =
    Rewrite(
      xs.map(f1).map(f2),
      xs.map(x => f1(f2(x)))
    )

  def isEmpty(x: Seq[Int]) = {
    Rewrite(
      x.length == 0,
      x.isEmpty
    )
  }

  def takeWhileAndMap(xs: Seq[Int], ptw: (Int => Boolean), f: (Int => Int)) =
    Rewrite(
      xs.takeWhile(ptw).map(f),
      xs.iterator.takeWhile(ptw).map(f).toVector
    )
  def takeWhileAndMap2(xs: Seq[Int], ptw: (Int => Boolean), f: (Int => Int)) =
    Rewrite(
      xs.takeWhile(ptw).map(f),
      xs.iterator.takeWhile(ptw).map(f).toSeq
    )

  
  def takeAndMap(xs: Seq[Int], f:(Int => Int), n: Int) = 
    Rewrite(
      xs.take(n).map(f),
      xs.iterator.take(n).map(f).toSeq
    )

    def mapAndTake2(xs: Seq[Int], f: (Int => Int), n: Int) =
    Rewrite(
      xs.map(f).take(n),
      {
        val it = xs.iterator
        val ret = it.map(f).take(n).toVector
        it.map(f).toVector
        ret
      }
    )

  def mapAndTake3(xs: Seq[Int], f: (Int => Int), n: Int) =
    Rewrite(
      xs.map(f).take(n),
      xs.iterator.map(f).take(n).toVector
    )


  def twoMaps(xs: Seq[Int], f1:(Int => Int), f2:(Int => Int)) =
    Rewrite(
      xs.map(f1).map(f2),
      xs.map(x => f2(f1(x)))
    )

  def mapAndTake(xs: Seq[Int], f: (Int => Int), n: Int) =
    Rewrite(
      xs.map(f).take(n),
      xs.iterator.map(f).take(n).toVector
    )

  def wrongTwoDropRights(xs: Seq[Int], a: Int, b: Int) = 
    Rewrite(
      xs.dropRight(a).dropRight(b),
      xs.dropRight(a + b)
    )

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
}
