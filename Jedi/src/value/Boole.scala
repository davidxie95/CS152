package value

import expression.Literal

case class Boole(val value: Boolean) extends Literal with Ordered[Boole] with Equals {

  def &&(other: Boole) = Boole(this.value && other.value)
  def ||(other: Boole) = Boole(this.value || other.value)
  def unary_! = Boole(!this.value)

  override def toString = value.toString
  def compare(other: Boole): Int = if (this.value < other.value) -1 else if (other.value < this.value) 1 else 0
  override def canEqual(other: Any) = other.isInstanceOf[Boole]

  override def equals(other: Any): Boolean =
    other match {
      case other: Boole => this.canEqual(other) && (other.value == this.value)
      case _            => false
    }
  override def hashCode = this.toString.##

}

object Boole {
  implicit def booleToText(n: Boole): Text = Text(n.value.toString)
}