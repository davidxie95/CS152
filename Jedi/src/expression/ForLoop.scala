package expression

import value._
import context._

case class ForLoop(lcv: Identifier, limit: Integer , body: Expression) extends SpecialForm {
   def execute(env: Environment): Value = {
     var tempEnv = new Environment(env)
     val counter = new Variable(Integer(0))
     tempEnv.put(lcv, counter)
     var result: Value = null
     while(counter.content.asInstanceOf[Integer].value < limit.value) {
       result = body.execute(tempEnv)
       counter.content = Integer(counter.content.asInstanceOf[Integer].value + 1)
     }
     result
   }
}