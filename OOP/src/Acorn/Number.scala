package Acorn

class Number(val num: Double) extends Expression {
  def execute = num
  
  override def toString = num.toString
}
object Number {
  def apply(num: Double) = new Number(num)
}