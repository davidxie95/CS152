package expression

import context.Environment

case class Block(exp: List[Expression]) extends SpecialForm {

  def execute(env: Environment) = {
    var temp = new Environment(env)
    val exe = exp.map(_.execute(temp))
    exe(exe.length - 1)
  }
}