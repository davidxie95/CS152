package expression

import context._
import value._

case class Access(obj: Expression, field: Identifier = null) extends SpecialForm {

  def execute(env: Environment): Value =
    {
	   if(obj.execute(env).isInstanceOf[Environment])
	     obj.execute(env).asInstanceOf[Environment].find(field)
	   else throw new TypeException("Object isn't of type environment")
    }

}