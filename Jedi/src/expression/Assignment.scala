package expression
import value._
import context.Environment
import context.TypeException

case class Assignment(vbl: Identifier, update: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    if (vbl.execute(env).isInstanceOf[Variable]) {
      vbl.execute(env).asInstanceOf[Variable].content = update.execute(env)
      Notification.OK
    } else throw new TypeException("Error: Not of type Variable")
  }
}