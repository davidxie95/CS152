package List_Processing
import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer

object List_Processing_II {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  //-------------------------------
  // problem 1

  var cs152: List[List[Double]] = List(List(93, 89, 90), List(75, 76, 68), List(88, 82, 78))
                                                  //> cs152  : List[List[Double]] = List(List(93.0, 89.0, 90.0), List(75.0, 76.0, 
                                                  //| 68.0), List(88.0, 82.0, 78.0))
  def avg(scores: List[Double]): Double = {
    scores.reduce(_ + _) / scores.length
  }                                               //> avg: (scores: List[Double])Double

  def avgAvg(scores: List[List[Double]]): List[Double] = {
    if (scores == Nil) Nil
    else scores.head.reduce(_ + _) / scores.head.length :: avgAvg(scores.tail)
  }                                               //> avgAvg: (scores: List[List[Double]])List[Double]

  def passing(scores: List[List[Double]]): List[Int] = {
    def helper(scores: List[List[Double]], index: Int): List[Int] = {
      if (scores == Nil) Nil
      //if avg is greater than 70, add to list
      else if (avg(scores.head) > 70) index :: helper(scores.tail, index + 1)
      else helper(scores.tail, index + 1)
    }
    helper(scores, 0)
  }                                               //> passing: (scores: List[List[Double]])List[Int]

  def sumSums(scores: List[List[Double]]): Double = {
    if (scores == Nil) 0
    else scores.head.reduce(_ + _) + sumSums(scores.tail)
  }                                               //> sumSums: (scores: List[List[Double]])Double

  for (i <- cs152) {
    println(avg(i))                               //> 90.66666666666667
                                                  //| 73.0
                                                  //| 82.66666666666667
  }
  avgAvg(cs152)                                   //> res0: List[Double] = List(90.66666666666667, 73.0, 82.66666666666667)
  passing(cs152)                                  //> res1: List[Int] = List(0, 1, 2)
  sumSums(cs152)                                  //> res2: Double = 739.0

  //-------------------------------
  // problem 2

  def spellCheck(doc: List[String], dictionary: List[String]): List[String] = {

    var temp = new ListBuffer[String]()
    for (i <- doc) {
      if (!dictionary.contains(i)) {
        temp += i
      }
    }
    val temp2 = temp.toList
    temp2
  }                                               //> spellCheck: (doc: List[String], dictionary: List[String])List[String]

  val list1 = List("apple", "banana", "cherry")   //> list1  : List[String] = List(apple, banana, cherry)
  val list2 = List("apple", "banana")             //> list2  : List[String] = List(apple, banana)

  spellCheck(list1, list2)                        //> res3: List[String] = List(cherry)

  //-------------------------------
  // problem 3

  def spellCheckMap(doc: List[String], dictionary: List[String]): List[String] = {
    doc.filter((i) => !dictionary.contains(i))
  }                                               //> spellCheckMap: (doc: List[String], dictionary: List[String])List[String]

  spellCheckMap(list1, list2)                     //> res4: List[String] = List(cherry)

  //-------------------------------
  // problem 4

  def evalMono(mono: (Double, Double), x: Double): Double = {
    mono._1 * (math.pow(x, mono._2))
  }                                               //> evalMono: (mono: (Double, Double), x: Double)Double

  def evalPoly(poly: List[(Double, Double)], x: Double): Double = {
    if (poly == Nil) 0
    else evalMono(poly.head, x) + evalPoly(poly.tail, x)
  }                                               //> evalPoly: (poly: List[(Double, Double)], x: Double)Double

  //testers
  val list = List((3.0, 2.0), (-5.0, 0.0))        //> list  : List[(Double, Double)] = List((3.0,2.0), (-5.0,0.0))
  // (3x^2) + (-5x^0)

  for (i <- list) {
    println(evalMono(i, 1))
    println(evalMono(i, 2))
  }                                               //> 3.0
                                                  //| 12.0
                                                  //| -5.0
                                                  //| -5.0
  evalPoly(list, 1)                               //> res5: Double = -2.0
  evalPoly(list, 2)                               //> res6: Double = 7.0
}