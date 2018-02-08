package expression

import context._
import value._

case class DoWhile(body: Expression, condition: Expression) extends SpecialForm {
	def execute(env: Environment): Value = {
	  def c = condition.execute(env);
	  var result: Value = Notification.UNKNOWN 
	  do
	  {
	    result = body.execute(env)
	  }while(c.asInstanceOf[Boole].value)
	    result
	}
}
