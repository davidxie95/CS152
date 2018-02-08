package expression

import value._
import context._
case class Object(locals: List[Expression], ext: Identifier = null) extends SpecialForm {
  
   def execute(env: Environment) = {
     var result: Environment = null
     if (ext == null)result = new Environment(env)
     else {
       val del = ext.execute(env)
       if (!del.isInstanceOf[Environment]) throw new TypeException("Extensions must be objects")
       result = new Environment(del.asInstanceOf[Environment])
     }
     for(e<-locals) e.execute(result)
     result
   }
}