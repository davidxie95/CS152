package Indus

class Item(val d: Description) {

  private var id: Int = Item.nextId
  override def toString = "(id: " + id + ", " + "Description: " + d + ")"

  def getId() = id

}
object Item {
  var count = 0
  def apply(d: Description) = {
    new Item(d)
  }

  def nextId = {
    count += 1
    count
  }

}