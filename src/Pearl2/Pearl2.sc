object Pearl2_A_surpassing_problem {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  // NAIVE SOLUTION //////////////////////////////////
  def tails[T](ls: List[T]): List[List[T]] = ls match {
  	case Nil => Nil
  	case xs => xs :: tails(xs.tail)
  }                                               //> tails: [T](ls: List[T])List[List[T]]
  
  tails(List(1,2,3))                              //> res0: List[List[Int]] = List(List(1, 2, 3), List(2, 3), List(3))
   
  def scount(elem: Char, ls: List[Char]): Int = ls.filter(_ > elem).length
                                                  //> scount: (elem: Char, ls: List[Char])Int
  def table(ls: List[Char]): List[(Char, Int)] = {
  	for(xs <- tails(ls)) yield (xs.head, scount(xs.head, xs))
  }                                               //> table: (ls: List[Char])List[(Char, Int)]
  
  def maxSurpasser(str: String): (Char, Int) ={
  	table(str.toList).maxBy(_._2)
  }                                               //> maxSurpasser: (str: String)(Char, Int)
  
  table("GENERATING".toList)                      //> res1: List[(Char, Int)] = List((G,5), (E,6), (N,2), (E,5), (R,1), (A,4), (T,
                                                  //| 0), (I,1), (N,0), (G,0))
  maxSurpasser("GENERATING")                      //> res2: (Char, Int) = (E,6)
  
  // RECURSIVE SOLUTION /////////////////////////////
  def tableRec(ls: List[Char]): List[(Char, Int)] = ls match {
  	case x :: Nil => List((x, 0))
  	case _ => merge(tableRec(ls.take(ls.length/2)), tableRec(ls.drop(ls.length/2)))
  }                                               //> tableRec: (ls: List[Char])List[(Char, Int)]
  
  def merge(xs: List[(Char, Int)], ys: List[(Char, Int)]): List[(Char, Int)] = (xs, ys) match {
  	case (xs, Nil) => xs
  	case (Nil, ys) => ys
  	case(xsx@(x :: xs), ysy@(y :: ys)) => if(x._1 >= y._1) y :: merge(xsx, ys) else (x._1, x._2 + ysy.length) :: merge(xs, ysy)
  }                                               //> merge: (xs: List[(Char, Int)], ys: List[(Char, Int)])List[(Char, Int)]
  
  tableRec("GENERATING".toList)                   //> res3: List[(Char, Int)] = List((A,4), (E,5), (E,6), (G,0), (G,5), (I,1), (N
                                                  //| ,0), (N,2), (R,1), (T,0))
  def maxSurpasserRec(str: String): (Char, Int) = {
  	tableRec(str.toList).maxBy(_._2)
  }                                               //> maxSurpasserRec: (str: String)(Char, Int)
  
  maxSurpasserRec("GENERATING")                   //> res4: (Char, Int) = (E,6)
}