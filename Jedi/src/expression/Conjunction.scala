package expression
import context._
import value._
import context.Environment

case class Conjunction(exp: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    def helper(exp2: List[Expression]): Boolean = {
      if (exp2 == Nil) true
      val a = exp2.head.execute(env)
      if (exp2.head.execute(env).isInstanceOf[Boole]) {
        if (a.toString == "true") helper(exp2.tail)
        else false
      } else throw new TypeException("Conjunction only allow Booles!")
    }
    new Boole(helper(exp))
  }
}