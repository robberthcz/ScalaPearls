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
  
  invert1( (x, y) => x + y, 10);System.out.println("""res1: List[(Int, Int)] = """ + $show(res$1));$skip(518); 
  
  // 2nd IMPROVEMENT ////////////////////////////
  def invert2(f: (Int, Int) => Int, z: Int): List[(Int, Int)] = {
  	def eliminate(row: Int, col: Int): List[(Int, Int)] = {
  		val fz = f(row, col)
  		if(row < 0 || col > z) Nil
  		// eliminate current row
  	  else if(fz > z) eliminate(row - 1, col)
  		// eliminate current row and column
  		else if(fz == z) (col, row) :: eliminate(row - 1, col + 1)
  		// fz < z
  		// eliminate current column
  		else eliminate(row, col + 1)
  	}
  	eliminate(z, 0)
  };System.out.println("""invert2: (f: (Int, Int) => Int, z: Int)List[(Int, Int)]""");$skip(34); val res$2 = 
  
  invert2((x,y) => x + y, 10 );System.out.println("""res2: List[(Int, Int)] = """ + $show(res$2))}
  
  
}
