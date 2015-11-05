package Pearl3

object Pearl3 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(234); 
  // NAIVE SOLUTION /////////////////////////////
  def invert(f: (Int, Int) => Int, z: Int): List[(Int, Int)] = {
  	for( x <- (0 to z).toList; y <- (0 to z).toList; if(f(x, y) == z) ) yield (x, y)
  };System.out.println("""invert: (f: (Int, Int) => Int, z: Int)List[(Int, Int)]""");$skip(34); val res$0 = 
  
  invert( (x, y) => x + y, 10);System.out.println("""res0: List[(Int, Int)] = """ + $show(res$0));$skip(372); 
  
  // 1st IMPROVEMENT ////////////////////////////
  // the number of evaluations will decrease by a factor of two
  // since f(x, y) is increasing => "f(x, y) >= x + y" => "x + y <= z""
  def invert1(f: (Int, Int) => Int, z: Int): List[(Int, Int)] = {
  	for{ x <- List.range(0, z + 1)
  			 y <- List.range(0, z - x + 1)
  			 if(f(x, y) == z)      } yield (x, y)
  };System.out.println("""invert1: (f: (Int, Int) => Int, z: Int)List[(Int, Int)]""");$skip(35); val res$1 = 
  
  invert1( (x, y) => x + y, 10);System.out.println("""res1: List[(Int, Int)] = """ + $show(res$1))}
  
  // 2nd IMPROVEMENT ////////////////////////////
  
  
  
}
