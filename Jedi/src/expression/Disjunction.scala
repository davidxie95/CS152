package expression
import context._
import value._
import context.Environment

case class Disjunction(exp: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    def helper(exp2: List[Expression]): Boolean = {
      if (exp2 == Nil) false
      val a = exp2.head.execute(env)
      if (exp2.head.execute(env).isInstanceOf[Boole]) {
        if (a.toString == "true") true
        else helper(exp2.tail)
      } else
        throw new TypeException("Disjunction only allow Booles!")
    }
    new Boole(helper(exp))
  }
}