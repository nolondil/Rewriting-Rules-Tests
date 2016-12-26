import dotty.linker._

@rewrites
object IntRules {
  def testMap(f1: Int => Int, xs: Seq[Int]) =
    Rewrite(
      xs.map(f1),
      xs.map(f1)
    )

  /*def twoMaps(f1: Int => Int, f2: Int => Int, xs: Seq[Int]) =
    Rewrite(
      xs.map(f1).map(f2),
      xs.map(x => f2(f1(x)))
    )*/
 /* def filterAndMap(xs: Seq[Int], p: (Int => Boolean), f: (Int => Int)) =
    Rewrite(
      xs.filter(p).map(f),
      {
        val it = xs.iterator
        def stream(iterator: Iterator[Int]): Stream[Int] =
          if (iterator.hasNext) {
            val next = iterator.next
            if (p(next))
              f(next) #:: stream(iterator)
            else
              stream(iterator)
          } else {
            Stream.Empty
          }
        stream(it).toSeq
      }
    )

  def mapAndFilter(xs: Seq[Int], f: (Int => Int), p: (Int => Boolean)) =
    Rewrite(
      xs.map(f).filter(p),
      {
        val it = xs.iterator
        def stream(iterator: Iterator[Int]): Stream[Int] =
          if (iterator.hasNext) {
            val next = f(iterator.next)
            if (p(next))
              next #:: stream(iterator)
            else
              stream(iterator)
          } else {
            Stream.Empty
          }
        stream(it).toSeq
      }
    )

  def takeWhileAndMap(xs: Seq[Int], p: (Int => Boolean), f: (Int => Int)) =
    Rewrite(
      xs.takeWhile(p).map(f),
      {
        val it = xs.iterator
        def stream(iterator: Iterator[Int]): Stream[Int] =
          if (iterator.hasNext) {
            val next = iterator.next
            if (p(next))
              f(next) #:: stream(iterator)
            else
              Stream.Empty
          } else {
            Stream.Empty
          }
        stream(it).toSeq
      }
    )

  def mapAndTakeWhile(xs: Seq[Int], p: (Int => Boolean), f: (Int => Int)) =
    Rewrite(
      xs.map(f).takeWhile(p),
      {
        val it = xs.iterator
        def stream(iterator: Iterator[Int]): Stream[Int] =
          if (iterator.hasNext) {
            val next = f(iterator.next)
            if (p(next))
              next #:: stream(iterator)
            else
              Stream.Empty
          } else {
            Stream.Empty
          }
        stream(it).toSeq
      }
    )

  private def rewriteTakeMap(xs: Seq[Int], n: Int, f: (Int => Int)) = {
    val it = xs.iterator
    def stream(i: Int, iterator: Iterator[Int]): Stream[Int] =
      if (iterator.hasNext && i < n)
        f(iterator.next) #:: stream(i + 1, iterator)
      else
        Stream.Empty
    stream(0, it).toSeq
  }

  def takeAndMap(xs: Seq[Int], n: Int, f: (Int => Int)) =
    Rewrite(
      xs.take(n).map(f),
      rewriteTakeMap(xs, n, f)
    )

  def mapAndTake(xs: Seq[Int], n: Int, f: (Int => Int)) =
    Rewrite(
      xs.map(f).take(n),
      rewriteTakeMap(xs, n, f)
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
        stream(it).toSeq
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
              else
                Stream.Empty
            else
              stream(iterator)
          } else {
            Stream.Empty
          }
        stream(it).toSeq
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
          stream(0, it).toSeq
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
            Stream.Empty
          }
        stream(0, it).toSeq
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
    )*/
}

/*object TestIntRules {
  def main(args: Array[String]): Unit = {
    def f1(i: Int) = 2*i
    def f2(i: Int) = 4 + i
    val sequence : Seq[Int] = (1 to 10)
    sequence.dropRight(2).dropRight(3)
    sequence.take(2).dropRight(4)
    sequence.take(2).filter(_ % 2 == 0)
    sequence.filter(_ % 2 == 0).take(2)
    println(sequence.map(f1).map(f2))
    println(sequence.length == 0)
    //List()*/
    /*List(1,2,3,4).map(x => 2*x).map(x => x + 4)
    List(1,2,3,4).filter(x => x % 2 == 0).map(x => x + 1)
    List(1,2,3,4).dropRight(1).dropRight(1)*/
//  }
//}
