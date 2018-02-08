
import scala.collection.immutable.List
import scala.collection.mutable

object Final {

  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  //sumFirst(List(List(1), Nil, List(2, 3, 4))) = 1 + 2 = 3
  
  def map(list: List[Int]) = if (list == Nil)0 else list.head
                                                  //> map: (list: List[Int])Int
  def sumFirst(list: List[List[Int]]): Int = {
    list.map(map).reduce (_ + _)
  }                                               //> sumFirst: (list: List[List[Int]])Int
sumFirst(List(List(1), Nil, List(2, 3, 4)))       //> res0: Int = 3
  
  
}