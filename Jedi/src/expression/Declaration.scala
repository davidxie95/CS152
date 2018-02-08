package expression
import context._
import value._
import context.Environment

case class Declaration(id: Identifier, exp: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {

    env.bulkPut(List(id), List(exp.execute(env)))
    Notification.OK
  }
}