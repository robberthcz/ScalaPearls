package Pearl9

object Pearl9 {
	// NAIVE SOLUTION //////////////////////////////
	// 2nd and 3rd person are celebrities, 1st and 4th are not
	// every person knows itself
	val knowsMatrix: Vector[Vector[Boolean]] = Vector(Vector(true, true, true, false), Vector(false, true, true, false), Vector(false, true, true, false), Vector(false, true, true, true))
                                                  //> knowsMatrix  : Vector[Vector[Boolean]] = Vector(Vector(true, true, true, fal
                                                  //| se), Vector(false, true, true, false), Vector(false, true, true, false), Vec
                                                  //| tor(false, true, true, true))
	def knows(x: Int, y: Int): Boolean = knowsMatrix(x - 1) (y - 1)
                                                  //> knows: (x: Int, y: Int)Boolean
	
	knows(2, 3)                               //> res0: Boolean = true
	knows(2, 1)                               //> res1: Boolean = false
	
	def subseqs[T](ls: List[T]): List[List[T]] = ls match {
		case Nil => List(Nil)
		case x :: xs => (subseqs(xs)).map(x :: _) ::: subseqs(xs)
	}                                         //> subseqs: [T](ls: List[T])List[List[T]]
	subseqs(List(1,2,3))                      //> res2: List[List[Int]] = List(List(1, 2, 3), List(1, 2), List(1, 3), List(1),
                                                  //|  List(2, 3), List(2), List(3), List())
	
	
	
	def subseqsFold[T](ls: List[T]): List[List[T]] = {
		ls.foldRight(List(List[T]())) { (x, z) => z.map(x :: _) ::: z}
	}                                         //> subseqsFold: [T](ls: List[T])List[List[T]]
	
	subseqsFold(List(1,2,3))                  //> res3: List[List[Int]] = List(List(1, 2, 3), List(1, 2), List(1, 3), List(1),
                                                  //|  List(2, 3), List(2), List(3), List())
	
	def isClique(p: => List[Int]) (c: => List[Int]): Boolean = {
		lazy val cs: Stream[Boolean] = for {
			x <- p.toStream
			y <- c.toStream
		}
		yield { println(knows(x, y) && ( !knows(y, x) || (knows(x,y) &&  c.contains(x))) )
						knows(x, y) && ( !knows(y, x) || (knows(x,y) &&  c.contains(x)) ) }
		
		// this step should be evaluated by lazy evaluation to make it more efficient
		and(cs)
	}                                         //> isClique: (p: => List[Int])(c: => List[Int])Boolean
	
	def and(ls: => Stream[Boolean]): Boolean = ls match{
		case Stream() => true
		case x #:: xs => if(x) and(xs) else false
	}                                         //> and: (ls: => Stream[Boolean])Boolean
	
	and(Stream(true, true, false, true))      //> res4: Boolean = false
	
	isClique(List(1,2,3,4)) (List(1,4))       //> true
                                                  //| false
                                                  //| false
                                                  //| res5: Boolean = false
	isClique(List(1,2,3,4)) (List(2,3))       //> true
                                                  //| true
                                                  //| true
                                                  //| true
                                                  //| true
                                                  //| true
                                                  //| true
                                                  //| true
                                                  //| res6: Boolean = true
	
	def clique1(p: List[Int]): List[Int] = subseqs(p).filter(isClique(p) (_) ).head
                                                  //> clique1: (p: List[Int])List[Int]
	
	//clique1(List(1,2,3,4))
}