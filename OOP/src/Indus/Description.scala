package Indus

class Description(val description: String, val price: Double, val supplier: String) {

  override def toString = "(Description: " + description + ", " + "price: " + price + ", " + "supplier: " + supplier + ")"

}
object Description {

  def apply(description: String, price: Double, supplier: String) =
    new Description(description, price, supplier)
}