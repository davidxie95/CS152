package MT

import scala.collection.immutable.List
import scala.collection.mutable

object Midterm {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  //problem 1
  def isFour(s: String) = if (s.length >= 4) true else false
                                                  //> isFour: (s: String)Boolean
  def position(s: String) = s.substring(2, 4)     //> position: (s: String)String
  def cat(word1: String, word2: String) = word1 + "" + word2
                                                  //> cat: (word1: String, word2: String)String

  def decode(list: List[String]): String = {
    list.filter(isFour _).map(position _).reduce (cat _)

  }                                               //> decode: (list: List[String])String

  decode(List("I hate", "to", "get", "back", "mid", "-terms"))
                                                  //> res0: String = hacker

  //problem 2

  class Item(val price: Double)
  class Toy(val min: Int, val max: Int, price: Double) extends Item(price)
  object Toy {
    def apply(min: Int, max: Int, price: Double) = new Toy(min, max, price)
  }
  class Book(val name: String, price: Double) extends Item(price)
  object Book {
    def apply(name: String, price: Double) = new Book(name, price)
  }
  class Cart {

    val list = new scala.collection.mutable.ArrayBuffer[Item]
    def add(i: Item) { list += i }
    def total: Double = { list.map(_.price).reduce(_ + _) }

  }

  val cart = new Cart                             //> cart  : MT.Midterm.Cart = MT.Midterm$$anonfun$main$1$Cart$1@21bcffb5
  cart.add(Toy(3, 5, 19.99))
  cart.add(Book("Scala for Poets", 22.99))
  cart.add(Book("OOP for Jocks", 18.25))
  cart.total                                      //> res1: Double = 61.23

  //problem 3

  def listRecur[T, S](nilVal: S, combiner: (T, S) => S): List[T] => S = {
    def f(x: List[T]): S = if (x == Nil) nilVal else combiner(x.head, f(x.tail))
    f _
  }                                               //> listRecur: [T, S](nilVal: S, combiner: (T, S) => S)List[T] => S
  //part B
  def sum = listRecur(0.0, (x: Double, y: Double) => x + y)
                                                  //> sum: => List[Double] => Double
  def concat = listRecur("", (x: String, y: String) => x + y)
                                                  //> concat: => List[String] => String

  sum(List(1.0, 2.0, 3.0))                        //> res2: Double = 6.0
  concat(List("say", "hat", "chin"))              //> res3: String = sayhatchin
}