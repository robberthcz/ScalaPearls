object Pearl6 {
  // sum of Terms => represents +
  type Expression = List[Term]
  // multiplication of factors => represents *
  type Term = List[Factor]
  // concatenation of digits => represents concat
  type Factor = List[Int]

  // methods which evaluate a given Factor, Term or an Expression
  def valueFactor(ls: Factor): Int =
  	ls.foldLeft(0)((z, x) => x + z * 10)      //> valueFactor: (ls: Pearl6.Factor)Int
  def valueTerm(ls: Term): Int =
  	ls.foldLeft(1)((z, x) => z * valueFactor(x))
                                                  //> valueTerm: (ls: Pearl6.Term)Int
  def valueExpression(ls: Expression): Int =
  	ls.foldLeft(0)((z, x) => valueTerm(x) + z)//> valueExpression: (ls: Pearl6.Expression)Int
  
  // methods for showing
  def showFactor(ls: Factor) =
  	valueFactor(ls).toString                  //> showFactor: (ls: Pearl6.Factor)String
  def showTerm(ls: Term) =
  	ls.map(showFactor(_)).mkString(" * ")     //> showTerm: (ls: Pearl6.Term)String
  def showExpression(ls: Expression) =
  	ls.map(showTerm(_)).mkString(" + ")       //> showExpression: (ls: Pearl6.Expression)String
  
  // TESTING
  val factor = List(1, 2, 3)                      //> factor  : List[Int] = List(1, 2, 3)
  val term = List(factor, List(1, 2))             //> term  : List[List[Int]] = List(List(1, 2, 3), List(1, 2))
  val expr = List(term, List(List(5), List(2)))   //> expr  : List[List[List[Int]]] = List(List(List(1, 2, 3), List(1, 2)), List(L
                                                  //| ist(5), List(2)))

  showFactor(factor)                              //> res0: String = 123
  showTerm(term)                                  //> res1: String = 123 * 12
  showExpression(expr)                            //> res2: String = 123 * 12 + 5 * 2
  valueFactor(factor)                             //> res3: Int = 123
  valueTerm(term)                                 //> res4: Int = 1476
  valueExpression(expr)                           //> res5: Int = 1486
  
  
  def solutions(ls: List[Int], n: Int): List[Expression] = {
  	val emptyExpression: Expression = List(List(Nil))
  	
  	def solve(ls: List[Int]): List[Expression] = {
  		ls.foldRight(List(emptyExpression))( (x, z) => z.flatMap(glue(x, _))).filter(good(_))
  	}
  	
  	def glue(elem: Int, e: Expression): List[Expression] = e match{
  		case (Nil :: Nil) :: Nil => List(List(List(List(elem))))
  		case (xs :: xss) :: xsss => List( ((elem :: xs) :: xss) :: xsss, ( List(elem) :: xs :: xss) :: xsss, List(List(elem)) :: (xs :: xss) :: xsss)
  	}
  	
  	def good(e: Expression): Boolean = valueExpression(e) == n
  	
  	def ok(e: Expression): Boolean = valueExpression(e) <= n
  	
  	solve(ls)
  }                                               //> solutions: (ls: List[Int], n: Int)List[Pearl6.Expression]
  
  def showSolutions(ls: List[Int], n: Int) = {
  	solutions(ls, n).map(showExpression(_))
  }                                               //> showSolutions: (ls: List[Int], n: Int)List[String]
  // should be 7 results
  showSolutions(List(1,2,3,4,5,6,7,8,9), 100)     //> res6: List[String] = List(1 * 2 * 3 + 4 + 5 + 6 + 7 + 8 * 9, 1 + 2 + 3 + 4 
                                                  //| + 5 + 6 + 7 + 8 * 9, 1 * 2 * 3 * 4 + 5 + 6 + 7 * 8 + 9, 12 + 3 * 4 + 5 + 6 
                                                  //| + 7 * 8 + 9, 1 + 2 * 3 + 4 + 5 + 67 + 8 + 9, 1 * 2 + 34 + 5 + 6 * 7 + 8 + 9
                                                  //| , 12 + 34 + 5 * 6 + 7 + 8 + 9)
}