package Pearl3

object Pearl3 {
  // NAIVE SOLUTION /////////////////////////////
  def invert(f: (Int, Int) => Int, z: Int): List[(Int, Int)] = {
  	for( x <- (0 to z).toList; y <- (0 to z).toList; if(f(x, y) == z) ) yield (x, y)
  }                                               //> invert: (f: (Int, Int) => Int, z: Int)List[(Int, Int)]
  
  invert( (x, y) => x + y, 10)                    //> res0: List[(Int, Int)] = List((0,10), (1,9), (2,8), (3,7), (4,6), (5,5), (6,
                                                  //| 4), (7,3), (8,2), (9,1), (10,0))
  
  // 1st IMPROVEMENT ////////////////////////////
  // the number of evaluations will decrease by a factor of two
  // since f(x, y) is increasing => "f(x, y) >= x + y" => "x + y <= z""
  def invert1(f: (Int, Int) => Int, z: Int): List[(Int, Int)] = {
  	for{ x <- List.range(0, z + 1)
  			 y <- List.range(0, z - x + 1)
  			 if(f(x, y) == z)      } yield (x, y)
  }                                               //> invert1: (f: (Int, Int) => Int, z: Int)List[(Int, Int)]
  
  invert1( (x, y) => x + y, 10)                   //> res1: List[(Int, Int)] = List((0,10), (1,9), (2,8), (3,7), (4,6), (5,5), (6,
                                                  //| 4), (7,3), (8,2), (9,1), (10,0))
  
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
  }                                               //> invert2: (f: (Int, Int) => Int, z: Int)List[(Int, Int)]
  
  invert2((x,y) => x + y, 10 )                    //> res2: List[(Int, Int)] = List((0,10), (1,9), (2,8), (3,7), (4,6), (5,5), (6
                                                  //| ,4), (7,3), (8,2), (9,1), (10,0))
  
  
}