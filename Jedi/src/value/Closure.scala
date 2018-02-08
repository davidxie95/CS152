package value

import context._
import expression._

class Closure(params: List[Identifier], body: Expression, defEnv: Environment) extends Value {
   def apply(args: List[Value], callEnv: Environment = null): Value = {
     val localEnv = 
       if (Closure.useStaticScopeRule) new Environment(defEnv) 
       else new Environment(callEnv) // null callEnv ok?
     localEnv.bulkPut(params, args)
     body.execute(localEnv)
   }
}

object Closure {
  var useStaticScopeRule = true
}