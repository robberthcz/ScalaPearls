package Pearl4

object Pearl4 {
  // NAIVE SOLUTION //////////////////////////////
  def smallest1(k: Int, xs: List[Int], ys: List[Int]): Int = nth(k, merge(xs, ys))
                                                  //> smallest1: (k: Int, xs: List[Int], ys: List[Int])Int
  // Returns nth element in a list, starting at 0
  def nth[T](n: Int, ls: List[T]): T = (n, ls) match {
    case (0, _)       => ls.head
    case (n, x :: xs) => nth(n - 1, xs)
    case (_, Nil)     => throw new NoSuchElementException
  }                                               //> nth: [T](n: Int, ls: List[T])T
  // Merges two sorted lists
  def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, ys)          => ys
    case (xs, Nil)          => xs
    case (x :: xs, y :: ys) => if (x < y) x :: merge(xs, y :: ys) else y :: merge(x :: xs, ys)
  }                                               //> merge: (xs: List[Int], ys: List[Int])List[Int]

  smallest1(2, List(2, 5, 7), List(1, 3, 4, 8))   //> res0: Int = 3

  // RECURSIVE SOLUTION ///////////////////////////
  def smallest2(k: Int, xs: List[Int], ys: List[Int]): Int = (xs, ys) match {
    case (Nil, ys) => nth(k, ys)
    case (xs, Nil) => nth(k, xs)
    case _ => {
      val p = xs.length / 2
      val q = ys.length / 2
      val (ss, a :: ts) = xs.splitAt(p)
      val (us, b :: vs) = ys.splitAt(q)
      (a < b, k <= p + q) match {
        case (true, true)   => smallest2(k, xs, us)
        case (true, false)  => smallest2(k - p - 1, ts, ys)
        case (false, false) => smallest2(k - q - 1, xs, vs)
        case (false, true)  => smallest2(k, ss, ys)
      }
    }
  }                                               //> smallest2: (k: Int, xs: List[Int], ys: List[Int])Int

  smallest2(2, List(2, 5, 7), List(1, 3, 4, 8))   //> res1: Int = 3

}