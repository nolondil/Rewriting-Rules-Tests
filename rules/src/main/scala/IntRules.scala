import dotty.linker._

@rewrites
object IntRules {

  def twoMaps(f1: Int => Int, f2: Int => Int, xs: Seq[Int]) =
    Rewrite(
      xs.map(f1).map(f2),
      xs.map(x => f2(f1(x)))
    )

  def twoDropRight(xs: Seq[Int], n: Int, m: Int) = 
    Rewrite(
      xs.dropRight(n).dropRight(m),
      {
        val n0 = Math.max(0, n)
        val m0 = Math.max(0, m)
        if ((n0/2 + m0/2) >= Int.MaxValue/2)
          Nil
        else
          xs.dropRight(n0 + m0)
      }
    )
  
  def filterAndMap(xs: Seq[Int], p: (Int => Boolean), f: (Int => Int)) =
    Rewrite(
      xs.filter(p).map(f),
      xs.iterator.filter(p).map(f).toVector
    )

  def mapAndFilter(xs: Seq[Int], f: (Int => Int), p: (Int => Boolean))(implicit xspure: IsPure[xs.type]) =
    Rewrite(
      xs.map(f).filter(p),
      xs.iterator.map(f).filter(p).toVector
    )

  def takeWhileAndMap(xs: Seq[Int], p: (Int => Boolean), f: (Int => Int)) =
    Rewrite(
      xs.takeWhile(p).map(f),
      xs.iterator.takeWhile(p).map(f).toVector  
    )

  def mapAndTakeWhile(xs: Seq[Int], ptw: (Int => Boolean), f: (Int => Int)) =
    Rewrite(
      xs.map(f).takeWhile(ptw),
      {
        val it = xs.iterator
        val ret = it.map(f).takeWhile(ptw).toVector
        it.map(f).toVector
        ret
      }
    )

  private def rewriteTakeMap(xs: Seq[Int], n: Int, f: (Int => Int), sideEffects: Boolean = false) = {
    val it = xs.iterator
    def stream(i: Int, iterator: Iterator[Int]): Stream[Int] =
      if (iterator.hasNext && i < n)
        f(iterator.next) #:: stream(i + 1, iterator)
      else {
        if (sideEffects)
          iterator.map(f).toVector
        Stream.Empty
      }
    stream(0, it).toVector
  }

  def takeAndMap(xs: Seq[Int], n: Int, f: (Int => Int)) =
    Rewrite(
      xs.take(n).map(f),
      rewriteTakeMap(xs, n, f)
    )

  def mapAndTake(xs: Seq[Int], n: Int, f: (Int => Int)) =
    Rewrite(
      xs.map(f).take(n),
      rewriteTakeMap(xs, n, f, true)
    )

  def takeWhileAndFilter(xs: Seq[Int], ptw: (Int => Boolean), pf: (Int => Boolean)) =
    Rewrite(
      xs.takeWhile(ptw).filter(pf),
      {
        val it = xs.iterator
        def stream(iterator: Iterator[Int]): Stream[Int] =
          if (iterator.hasNext) {
            val next = iterator.next
            if (ptw(next))
              if (pf(next))
                next #:: stream(iterator)
              else
                stream(iterator)
            else
              Stream.Empty
          }
          else Stream.Empty
        stream(it).toVector
      }
    )

  def filterAndTakeWhile(xs: Seq[Int], ptw: (Int => Boolean), pf: (Int => Boolean)) =
    Rewrite(
      xs.filter(pf).takeWhile(ptw),
      {
        val it = xs.iterator
        def stream(iterator: Iterator[Int]): Stream[Int] =
          if (iterator.hasNext) {
            val next = iterator.next
            if (pf(next))
              if (ptw(next))
                next #:: stream(iterator)
              else {
                while(iterator.hasNext) {
                  pf(iterator.next)
                }
                Stream.Empty
              }
            else
              stream(iterator)
          } else {
            Stream.Empty
          }
        stream(it).toVector
      }
    )
  
  def takeAndFilter(xs: Seq[Int], n: Int, pf: (Int => Boolean)) =
    Rewrite(
      xs.take(n).filter(pf),
      {
        val it : Iterator[Int] = xs.iterator
        def stream(i: Int, iterator: Iterator[Int]): Stream[Int] =
          if (iterator.hasNext && i < n) {
            val next = iterator.next
            if (pf(next))
              next #::stream(i+1, iterator)
            else
              stream(i+1, iterator)
          }
          else Stream.Empty
          stream(0, it).toVector
      }
    )

  def filterAndTake(xs: Seq[Int], n: Int, pf: (Int => Boolean)) =
    Rewrite(
      xs.filter(pf).take(n),
      {
        val it = xs.iterator
        def stream(i: Int, iterator: Iterator[Int]): Stream[Int] =
          if (iterator.hasNext && i < n) {
            val next = iterator.next
            if (pf(next)) {
              next #::stream(i+1, iterator)
            } else {
              stream(i, iterator)
            }
          } else {
            while (iterator.hasNext) {
              pf(iterator.next)
            }
            Stream.Empty
          }
        stream(0, it).toVector
      }
    )
  
  def mapAndFoldLeft(xs: Seq[Int], f: (Int => Int), op: ((Int, Int) => Int), initial: Int) =
    Rewrite(
      xs.map(f).foldLeft(initial)(op),
      xs.foldLeft(initial)((a, b) => op(a, f(b)))
    )
  
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
}

object ExampleRules {
    def main(args: Array[String]): Unit = {
    val xs: Seq[Int] = (1 to 15)
    def f1(i: Int) = i*2
    def f2(i: Int) = i + 2
    def pf(i: Int) = (i % 2) == 0
    def ptw(i: Int) = (i % 3) == 0
    def op(a: Int, b: Int) = a + b
    println("Two maps: " + xs.map(f1).map(f2))
    println("filter and map: " + xs.filter(pf).map(f1))
    println("Map and filter: " + xs.map(f1).filter(pf))
    println("takeWhile and map: " + xs.takeWhile(ptw).map(f1))
    println("Map and takeWhile: " + xs.map(f1).takeWhile(ptw)) 
    //println("take and map: " + xs.take(2).map(f1))
    //println("Map and take: " + xs.map(f1).take(2))
    println("TakeWhile and filter: " + xs.takeWhile(ptw).filter(pf))
    println("Filter and takeWhile: " + xs.filter(pf).takeWhile(ptw))
    //println("Take and filter: " + xs.take(2).filter(pf))
    //println("Filter and take: " + xs.filter(pf).take(2))
    println("Map and FoldLeft: " + xs.map(f1).foldLeft(0)(op))
    println("isEmpty: " + (xs.length == 0))
    println("Two dropRight: " + xs.dropRight(2).dropRight(1))
    println("dropRight and drop: "+ xs.dropRight(2).drop(1))
    println("drop and dropRight: " + xs.drop(1).dropRight(2))
    println("take and dropRight: " + xs.take(2).dropRight(1))
  }
}
