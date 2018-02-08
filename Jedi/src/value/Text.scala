package value

import expression.Literal

case class Text(val value: String) extends Literal with Ordered[Text] with Equals {
  def +(other: Text) = Text(this.value + other.value)
  def substring(i: Integer, j : Integer) = value.substring(i.value, j.value)

  override def toString = value.toString
  def compare(other: Text): Int = if (this.value < other.value) -1 else if (other.value < this.value) 1 else 0
  override def canEqual(other: Any) = other.isInstanceOf[Text]
  
  override def equals(other: Any): Boolean =
    other match {
      case other: Text => this.canEqual(other) && (other.value == this.value)
      case _              => false
    }
  override def hashCode = this.toString.##
}

object Text {
  //implicit def textToReal(n: Text): Real = Real(n.value.toString.toDouble)
}