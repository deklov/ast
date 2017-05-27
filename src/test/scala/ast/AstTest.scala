package ast

import org.scalatest._

class AstTest extends FunSuite with Matchers {
  val ast: Ast = Binary(Mul, Binary(Add, Number(1), Number(2)), Binary(Add, Number(3), Variable("x")))
  
  test("reduce") {
    assert(ast.reduce(Printer) == "(1 + 2) * (3 + x)")
    
    val env = Map[String, Int]("x" -> 4)
    assert(ast.reduce(Evaluator)(env) == 21)
  }
  
  test("map") {
    val inverters = new Mappers {
      def binary(o: Operator): Operator = if (o == Add) Mul else Add
      def number(n: Int): Int = n
      def variable(n: String): String = "y"
    }
    
    assert(ast.map(inverters).reduce(Printer) == "(1 * 2 + 3 * y)")
  }
}
