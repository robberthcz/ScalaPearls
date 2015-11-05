object Pearl1_The_smallest_free_number {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(181); 
	// NAIVE SOLUTION /////////////////////////
  def minfreeNaive(ls: List[Int]): Int = {
  	(Stream.from(0).filter(!ls.contains(_))).head
  };System.out.println("""minfreeNaive: (ls: List[Int])Int""");$skip(34); val res$0 = 
  
	minfreeNaive(List(1,2,0,4,5));System.out.println("""res0: Int = """ + $show(res$0));$skip(33); val res$1 = 
	minfreeNaive(List(1,2,0,4,5,3));System.out.println("""res1: Int = """ + $show(res$1));$skip(414); 
  
  // ARRAY-BASED SOLUTION ///////////////////
  def checklist(ls: List[Int]): Vector[Boolean] ={
  	def checklistSub(vec: Vector[Boolean], ls: List[Int]): Vector[Boolean] = ls match{
  		case Nil => vec
  		case _ => checklistSub(vec.updated(ls.head, true), ls.tail)
  		case _ => checklistSub(vec, ls.tail)
  	}
  	
  	val vec = Vector.fill(ls.length)(false)
  	checklistSub(vec, ls.filter(_ < ls.length))
  };System.out.println("""checklist: (ls: List[Int])Vector[Boolean]""");$skip(30); val res$2 = 
  
  checklist(List(1,2,4,5));System.out.println("""res2: Vector[Boolean] = """ + $show(res$2));$skip(126); 
  
  def minfreeArray(ls: List[Int]): Int = {
  	val vec: Vector[Boolean] = checklist(ls)
  	vec.takeWhile(x => x).length
  };System.out.println("""minfreeArray: (ls: List[Int])Int""");$skip(35); val res$3 = 
  
  minfreeArray(List(1,2,0,4,5));System.out.println("""res3: Int = """ + $show(res$3));$skip(854); 
  
  
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
  };System.out.println("""minfreeRec: (ls: List[Int])Int""");$skip(47); val res$4 = 
  // should be 3
  minfreeRec(List(1,2,0,4,5));System.out.println("""res4: Int = """ + $show(res$4));$skip(49); val res$5 = 
  // should be 6
  minfreeRec(List(5,3,0,1,4,2));System.out.println("""res5: Int = """ + $show(res$5))}
}
