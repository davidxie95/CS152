package expression

import context._
import value._

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {
 
  def execute(env: Environment): Value = {
    val args = operands.map(_.execute(env))
    try {
      val maybeClosure = operator.execute(env)
      if (maybeClosure.isInstanceOf[Closure]) {
        val closure = maybeClosure.asInstanceOf[Closure]
        closure(args, env)
      } else {
        throw new TypeException("Only functions can be called")
      }
    } catch {
      case e: UndefinedException => alu.execute(operator, args) 
    }
  }
}