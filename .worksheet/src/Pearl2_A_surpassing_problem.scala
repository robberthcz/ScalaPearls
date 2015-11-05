object Pearl2_A_surpassing_problem {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(205); 
  // NAIVE SOLUTION //////////////////////////////////
  def tails[T](ls: List[T]): List[List[T]] = ls match {
  	case Nil => Nil
  	case xs => xs :: tails(xs.tail)
  };System.out.println("""tails: [T](ls: List[T])List[List[T]]""");$skip(24); val res$0 = 
  
  tails(List(1,2,3));System.out.println("""res0: List[List[Int]] = """ + $show(res$0));$skip(79); 
   
  def scount(elem: Char, ls: List[Char]): Int = ls.filter(_ > elem).length;System.out.println("""scount: (elem: Char, ls: List[Char])Int""");$skip(116); 
  def table(ls: List[Char]): List[(Char, Int)] = {
  	for(xs <- tails(ls)) yield (xs.head, scount(xs.head, xs))
  };System.out.println("""table: (ls: List[Char])List[(Char, Int)]""");$skip(88); 
  
  def maxSurpasser(str: String): (Char, Int) ={
  	table(str.toList).maxBy(_._2)
  };System.out.println("""maxSurpasser: (str: String)(Char, Int)""");$skip(32); val res$1 = 
  
  table("GENERATING".toList);System.out.println("""res1: List[(Char, Int)] = """ + $show(res$1));$skip(29); val res$2 = 
  maxSurpasser("GENERATING");System.out.println("""res2: (Char, Int) = """ + $show(res$2));$skip(240); 
  
  // RECURSIVE SOLUTION /////////////////////////////
  def tableRec(ls: List[Char]): List[(Char, Int)] = ls match {
  	case x :: Nil => List((x, 0))
  	case _ => merge(tableRec(ls.take(ls.length/2)), tableRec(ls.drop(ls.length/2)))
  };System.out.println("""tableRec: (ls: List[Char])List[(Char, Int)]""");$skip(278); 
  
  def merge(xs: List[(Char, Int)], ys: List[(Char, Int)]): List[(Char, Int)] = (xs, ys) match {
  	case (xs, Nil) => xs
  	case (Nil, ys) => ys
  	case(xsx@(x :: xs), ysy@(y :: ys)) => if(x._1 >= y._1) y :: merge(xsx, ys) else (x._1, x._2 + ysy.length) :: merge(xs, ysy)
  };System.out.println("""merge: (xs: List[(Char, Int)], ys: List[(Char, Int)])List[(Char, Int)]""");$skip(35); val res$3 = 
  
  tableRec("GENERATING".toList);System.out.println("""res3: List[(Char, Int)] = """ + $show(res$3));$skip(92); 
  def maxSurpasserRec(str: String): (Char, Int) = {
  	tableRec(str.toList).maxBy(_._2)
  };System.out.println("""maxSurpasserRec: (str: String)(Char, Int)""");$skip(35); val res$4 = 
  
  maxSurpasserRec("GENERATING");System.out.println("""res4: (Char, Int) = """ + $show(res$4))}
}
