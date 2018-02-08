package value

import context._
import expression._

class Notification(msg: String) extends Value {
  override def toString = msg
}
object Notification {

  def UNKNOWN = new Notification("Unknown")
  def ERROR = new Notification("Error")
  def OK = new Notification("ok")
  def DONE = new Notification("done")
  def UNSPECIFIED = new Notification("UNSPECIFIED")

}