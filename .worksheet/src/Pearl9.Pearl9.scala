package Pearl9

object Pearl9 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(356); 
	// NAIVE SOLUTION //////////////////////////////
	// 2nd and 3rd person are celebrities, 1st and 4th are not
	// every person knows itself
	val knowsMatrix: Vector[Vector[Boolean]] = Vector(Vector(true, true, true, false), Vector(false, true, true, false), Vector(false, true, true, false), Vector(false, true, true, true));System.out.println("""knowsMatrix  : Vector[Vector[Boolean]] = """ + $show(knowsMatrix ));$skip(65); 
	def knows(x: Int, y: Int): Boolean = knowsMatrix(x - 1) (y - 1);System.out.println("""knows: (x: Int, y: Int)Boolean""");$skip(15); val res$0 = 
	
	knows(2, 3);System.out.println("""res0: Boolean = """ + $show(res$0));$skip(13); val res$1 = 
	knows(2, 1);System.out.println("""res1: Boolean = """ + $show(res$1));$skip(146); 
	
	def subseqs[T](ls: List[T]): List[List[T]] = ls match {
		case Nil => List(Nil)
		case x :: xs => (subseqs(xs)).map(x :: _) ::: subseqs(xs)
	};System.out.println("""subseqs: [T](ls: List[T])List[List[T]]""");$skip(22); val res$2 = 
	subseqs(List(1,2,3));System.out.println("""res2: List[List[Int]] = """ + $show(res$2));$skip(126); 
	
	
	
	def subseqsFold[T](ls: List[T]): List[List[T]] = {
		ls.foldRight(List(List[T]())) { (x, z) => z.map(x :: _) ::: z}
	};System.out.println("""subseqsFold: [T](ls: List[T])List[List[T]]""");$skip(28); val res$3 = 
	
	subseqsFold(List(1,2,3));System.out.println("""res3: List[List[Int]] = """ + $show(res$3));$skip(400); 
	
	def isClique(p: => List[Int]) (c: => List[Int]): Boolean = {
		lazy val cs: Stream[Boolean] = for {
			x <- p.toStream
			y <- c.toStream
		}
		yield { println(knows(x, y) && ( !knows(y, x) || (knows(x,y) &&  c.contains(x))) )
						knows(x, y) && ( !knows(y, x) || (knows(x,y) &&  c.contains(x)) ) }
		
		// this step should be evaluated by lazy evaluation to make it more efficient
		and(cs)
	};System.out.println("""isClique: (p: => List[Int])(c: => List[Int])Boolean""");$skip(127); 
	
	def and(ls: => Stream[Boolean]): Boolean = ls match{
		case Stream() => true
		case x #:: xs => if(x) and(xs) else false
	};System.out.println("""and: (ls: => Stream[Boolean])Boolean""");$skip(40); val res$4 = 
	
	and(Stream(true, true, false, true));System.out.println("""res4: Boolean = """ + $show(res$4));$skip(39); val res$5 = 
	
	isClique(List(1,2,3,4)) (List(1,4));System.out.println("""res5: Boolean = """ + $show(res$5));$skip(37); val res$6 = 
	isClique(List(1,2,3,4)) (List(2,3));System.out.println("""res6: Boolean = """ + $show(res$6));$skip(83); 
	
	def clique1(p: List[Int]): List[Int] = subseqs(p).filter(isClique(p) (_) ).head;System.out.println("""clique1: (p: List[Int])List[Int]""")}
	
	//clique1(List(1,2,3,4))
}
