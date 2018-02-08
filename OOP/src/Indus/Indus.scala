package Indus

import scala.collection.mutable.Map

object Indus {

  def main(args: Array[String]): Unit = {

    var inventory = scala.collection.mutable.LinkedHashMap[Item, Int]()

    for (i <- 0 until 5) { inventory(new Item(new Description("The Matrix DVD", 15.50, "DVD World"))) = Item.count }
    for (i <- 0 until 3) { inventory(new Item(new Description("The Terminator DVD", 13.25, "DVD World"))) = Item.count }
    for (i <- 0 until 2) { inventory(new Item(new Description("Ironman DVD", 18.00, "DVD Planet"))) = Item.count }

    for (i <- inventory) {
      println(i.toString)
    }
  }
}