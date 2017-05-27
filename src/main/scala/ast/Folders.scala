package ast

object Printer extends Folders[String] {
  def binary(o: Operator, l: String, r: String): String = o match {
    case Add => "(%s + %s)".format(l, r)
    case Mul => "%s * %s".format(l, r)
  }
  
  def number(n: Int): String = "%d".format(n)
  
  def variable(n: String): String = n
}

object Evaluator extends Folders[Map[String, Int] => Int] {
  def binary(o: Operator, l: R , r: R): R = o match {
    case Add => env => l(env) + r(env)
    case Mul => env => l(env) * r(env)
  }
  
  def number(n: Int): R = _ => n
  
  def variable(n: String): R = env => env.get(n).get
}
