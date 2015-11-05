object Pearl6 {
  // sum of Terms => represents +
  type Expression = List[Term]
  // multiplication of factors => represents *
  type Term = List[Factor]
  // concatenation of digits => represents concat
  type Factor = List[Int];import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(375); 

  // methods which evaluate a given Factor, Term or an Expression
  def valueFactor(ls: Factor): Int =
  	ls.foldLeft(0)((z, x) => x + z * 10);System.out.println("""valueFactor: (ls: Pearl6.Factor)Int""");$skip(81); 
  def valueTerm(ls: Term): Int =
  	ls.foldLeft(1)((z, x) => z * valueFactor(x));System.out.println("""valueTerm: (ls: Pearl6.Term)Int""");$skip(91); 
  def valueExpression(ls: Expression): Int =
  	ls.foldLeft(0)((z, x) => valueTerm(x) + z);System.out.println("""valueExpression: (ls: Pearl6.Expression)Int""");$skip(87); 
  
  // methods for showing
  def showFactor(ls: Factor) =
  	valueFactor(ls).toString;System.out.println("""showFactor: (ls: Pearl6.Factor)String""");$skip(68); 
  def showTerm(ls: Term) =
  	ls.map(showFactor(_)).mkString(" * ");System.out.println("""showTerm: (ls: Pearl6.Term)String""");$skip(78); 
  def showExpression(ls: Expression) =
  	ls.map(showTerm(_)).mkString(" + ");System.out.println("""showExpression: (ls: Pearl6.Expression)String""");$skip(45); 
  
  // TESTING
  val factor = List(1, 2, 3);System.out.println("""factor  : List[Int] = """ + $show(factor ));$skip(38); 
  val term = List(factor, List(1, 2));System.out.println("""term  : List[List[Int]] = """ + $show(term ));$skip(48); 
  val expr = List(term, List(List(5), List(2)));System.out.println("""expr  : List[List[List[Int]]] = """ + $show(expr ));$skip(23); val res$0 = 

  showFactor(factor);System.out.println("""res0: String = """ + $show(res$0));$skip(17); val res$1 = 
  showTerm(term);System.out.println("""res1: String = """ + $show(res$1));$skip(23); val res$2 = 
  showExpression(expr);System.out.println("""res2: String = """ + $show(res$2));$skip(22); val res$3 = 
  valueFactor(factor);System.out.println("""res3: Int = """ + $show(res$3));$skip(18); val res$4 = 
  valueTerm(term);System.out.println("""res4: Int = """ + $show(res$4));$skip(24); val res$5 = 
  valueExpression(expr);System.out.println("""res5: Int = """ + $show(res$5));$skip(703); 
  
  
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
  };System.out.println("""solutions: (ls: List[Int], n: Int)List[Pearl6.Expression]""");$skip(97); 
  
  def showSolutions(ls: List[Int], n: Int) = {
  	solutions(ls, n).map(showExpression(_))
  };System.out.println("""showSolutions: (ls: List[Int], n: Int)List[String]""");$skip(71); val res$6 = 
  // should be 7 results
  showSolutions(List(1,2,3,4,5,6,7,8,9), 100);System.out.println("""res6: List[String] = """ + $show(res$6))}
}
