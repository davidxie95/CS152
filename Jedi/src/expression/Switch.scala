package expression

import context._
import value._

case class Switch(selector: Expression, operands: List[Expression]) extends SpecialForm {
  def execute(env: Environment) = {
    val select = selector.execute(env)
    if (!select.isInstanceOf[Number]) { throw new TypeException("Must be a Integer") }
    else if (select.asInstanceOf[Integer] < Integer(operands.length)) {
      Notification.UNSPECIFIED
    } else {
      operands(select.asInstanceOf[Integer].value).execute(env)
    }
  }
}


