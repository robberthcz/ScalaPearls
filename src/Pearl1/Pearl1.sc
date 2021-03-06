object Pearl1_The_smallest_free_number {
	// NAIVE SOLUTION /////////////////////////
  def minfreeNaive(ls: List[Int]): Int = {
  	(Stream.from(0).filter(!ls.contains(_))).head
  }
  
	minfreeNaive(List(1,2,0,4,5))
	minfreeNaive(List(1,2,0,4,5,3))
  
  // ARRAY-BASED SOLUTION ///////////////////
  def checklist(ls: List[Int]): Vector[Boolean] ={
  	def checklistSub(vec: Vector[Boolean], ls: List[Int]): Vector[Boolean] = ls match{
  		case Nil => vec
  		case _ => checklistSub(vec.updated(ls.head, true), ls.tail)
  		case _ => checklistSub(vec, ls.tail)
  	}
  	
  	val vec = Vector.fill(ls.length)(false)
  	checklistSub(vec, ls.filter(_ < ls.length))
  }
  
  checklist(List(1,2,4,5))
  
  def minfreeArray(ls: List[Int]): Int = {
  	val vec: Vector[Boolean] = checklist(ls)
  	vec.takeWhile(x => x).length
  }
  
  minfreeArray(List(1,2,0,4,5))
  
  
  // RECURSIVE SOLUTION //////////////////////
  // this solution requires a list with only distinct elements
  def minfreeRec(ls: List[Int]): Int = {
  	def minfreeSub(a: Int, ls: List[Int]): Int = {
  		// select b so as to have xs and ys close in their length
  		//ensures we recurse approximately on half of the array on average
  		val b = a + 1 + (ls.length/2)
  		val (xs, ys) = ls.partition(_ < b)
  		// if List(), then 0
  		if(ls.isEmpty) a
  		// the left subarray contains successive elements [a, a+1,...,b-1] => there is (b-a) of them
  		// it means the minfree number must be in the right subarray
  		else if( xs.length == b - a){
  			minfreeSub(b, ys)
  		// minfree must be in the left subarray, there is "gap" in potential sequence of numbers [a, a+1,...,b-1]
  		}
  		else minfreeSub(a, xs)
  	}
  
  	minfreeSub(0, ls)
  }
  // should be 3
  minfreeRec(List(1,2,0,4,5))
  // should be 6
  minfreeRec(List(5,3,0,1,4,2))
}