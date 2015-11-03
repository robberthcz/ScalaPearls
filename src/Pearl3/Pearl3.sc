package Pearl3

object Pearl3 {
  // NAIVE SOLUTION /////////////////////////////
  def invert(f: (Int, Int) => Int, z: Int): List[(Int, Int)] = {
  	for( x <- (0 to z).toList; y <- (0 to z).toList; if(f(x, y) == z) ) yield (x, y)
  }                                               //> invert: (f: (Int, Int) => Int, z: Int)List[(Int, Int)]
  
  invert( (x, y) => x*y, 25)                      //> res0: List[(Int, Int)] = List((1,25), (5,5), (25,1))
  
  // 1st IMPROVEMENT ////////////////////////////
  // the number of evaluations will decrease by a factor of two
  // since f(x, y) is increasing => "f(x, y) >= x + y" => "x + y <= z""
  def invert1(f: (Int, Int) => Int, z: Int): List[(Int, Int)] = {
  	for{ x <- List.range(0, z) 
  			 y <- List.range(0, z)
  			 if(f(x, y) == z)      } yield (x, y)
  }                                               //> invert1: (f: (Int, Int) => Int, z: Int)List[(Int, Int)]
  
  invert1( (x, y) => x*y, 25)                     //> res1: List[(Int, Int)] = List((5,5))
}