package ast

trait Folders[A] {
  type R = A
  
  def binary(o: Operator, l: A, r: A): A
  def number(n: Int): A
  def variable(n: String): A
}

trait Mappers {
  def binary(o: Operator): Operator
  def number(n: Int): Int
  def variable(n: String): String
}

trait Ast {
  def reduce[A](ops: Folders[A]): A
  
  def map(ops: Mappers): Ast = reduce(new Folders[Ast] {
    def binary(o: Operator, l: Ast, r:  Ast): Ast =
      Binary(ops.binary(o), l.asInstanceOf[Expression], r.asInstanceOf[Expression])
    
    def number(n: Int): Ast =
      Number(ops.number(n))
    
    def variable(n: String): Ast =
      Variable(ops.variable(n))    
  })
}

trait Expression {
  def reduce[A](ops: Folders[A]): A = this match {
    case Binary(o, l, r) => ops.binary(o, l.reduce(ops), r.reduce(ops))
    case Number(n)       => ops.number(n)
    case Variable(n)     => ops.variable(n)
  }
}
case class Binary(o: Operator, l: Expression, r: Expression) extends Expression with Ast
case class Number(v: Int) extends Expression with Ast
case class Variable(n: String) extends Expression with Ast

trait Operator
object Add extends Operator
object Mul extends Operator
