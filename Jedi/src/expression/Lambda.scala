package expression

import context.Environment
import value._

case class Lambda(param: List[Identifier], exp: Expression) extends SpecialForm {

  def execute(env: Environment): Value = {
    new Closure(param, exp, env)
  }
}