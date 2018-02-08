package expression

import value._
import context.Environment
import context.TypeException

case class Iteration(condition: Expression, body: Expression, testAtTop: Boolean = true) extends SpecialForm {

  def execute(env: Environment) = {
    var result: Value = if (testAtTop) Notification.UNSPECIFIED else body.execute(env)
    val c = condition.execute(env)
    if (!c.isInstanceOf[Boole]) throw new TypeException("while condition must be a Boole")
    var c2 = c.asInstanceOf[Boole]
    while (c2.value) {
      result = body.execute(env)
      c2 = condition.execute(env).asInstanceOf[Boole]
    }
    result
  }
}