package Pearl4

object Pearl4 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(166); 
  // NAIVE SOLUTION //////////////////////////////
  def smallest1(k: Int, xs: List[Int], ys: List[Int]): Int = nth(k, merge(xs, ys));System.out.println("""smallest1: (k: Int, xs: List[Int], ys: List[Int])Int""");$skip(240); 
  // Returns nth element in a list, starting at 0
  def nth[T](n: Int, ls: List[T]): T = (n, ls) match {
    case (0, _)       => ls.head
    case (n, x :: xs) => nth(n - 1, xs)
    case (_, Nil)     => throw new NoSuchElementException
  };System.out.println("""nth: [T](n: Int, ls: List[T])T""");$skip(268); 
  // Merges two sorted lists
  def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, ys)          => ys
    case (xs, Nil)          => xs
    case (x :: xs, y :: ys) => if (x < y) x :: merge(xs, y :: ys) else y :: merge(x :: xs, ys)
  };System.out.println("""merge: (xs: List[Int], ys: List[Int])List[Int]""");$skip(50); val res$0 = 

  smallest1(2, List(2, 5, 7), List(1, 3, 4, 8));System.out.println("""res0: Int = """ + $show(res$0));$skip(638); 

  // RECURSIVE SOLUTION USING LISTS ///////////////////////////
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
  };System.out.println("""smallest2: (k: Int, xs: List[Int], ys: List[Int])Int""");$skip(50); val res$1 = 

  smallest2(2, List(2, 5, 7), List(1, 3, 4, 8));System.out.println("""res1: Int = """ + $show(res$1));$skip(946); 
	
	// RECURSIVE SOLUTIONS USING ARRAY //////////////////////////////
	// the immutable array in Scala is represented by the Vector
	def smallest3(k: Int, x: List[Int], y: List[Int]): Int = {
		def search(n: Int, xs: Vector[Int], ys: Vector[Int], xs_lo: Int, xs_hi: Int, ys_lo: Int, ys_hi: Int): Int = {
			println("(" + xs_lo + ", " + xs_hi + "), (" + ys_lo + ", " + ys_hi +  "), " + n)
			if(xs_lo >= xs_hi) ys(n)
			else if(ys_lo >= ys_hi) xs(n)
			else{
				val mx = (xs_lo + xs_hi) / 2
				val my = (ys_lo + ys_hi) / 2
				(xs(mx) < ys(my), n <= mx + my) match {
					case (true, true) => search(n, xs, ys, xs_lo, xs_hi, ys_lo, my)
					case (true, false) => search(n - mx - 1, xs, ys, mx, xs_hi, ys_lo, ys_hi)
					case (false, true) => search(n, xs, ys, xs_lo, mx, ys_lo, ys_hi)
					case (false, false) => search(n - my - 1, xs, ys, xs_lo, xs_hi, my, ys_hi)
				}
			}
		}
		search(k, x.toVector, y.toVector,  0, x.length, 0, y.length)
	};System.out.println("""smallest3: (k: Int, x: List[Int], y: List[Int])Int""");$skip(49); val res$2 = 
	
	smallest3(2, List(2, 5, 7), List(1, 3, 4, 8));System.out.println("""res2: Int = """ + $show(res$2))}
}
