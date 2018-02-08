package expression
import context._
import value._

case class Identifier(val name: String) extends Expression with Serializable {
  override def toString = name
  def execute(env: Environment): Value = {
    val temp = env.apply(this)
    temp
  }
}
