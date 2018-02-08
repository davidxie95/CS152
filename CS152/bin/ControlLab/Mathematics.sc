package ControlLab

import scala.math._
import scala.util.Random

object Mathematics {

  //-------------------------------
  // problem 1

  def solve(a: Double, b: Double, c: Double) = {
    val disc = b * b - 4 * a * c
    if (disc < 0) None
    else {
      val root1 = (-b + Math.sqrt(disc)) / (2 * a)
      val root2 = (-b - Math.sqrt(disc)) / (2 * a)
      Some((root1, root2))
    }

  }                                               //> solve: (a: Double, b: Double, c: Double)Option[(Double, Double)]

  solve(2, -2, -4)                                //> res0: Option[(Double, Double)] = Some((2.0,-1.0))
  solve(1, 0, 1)                                  //> res1: Option[(Double, Double)] = None
  solve(1, 0, -1)                                 //> res2: Option[(Double, Double)] = Some((1.0,-1.0))
  solve(2, -6, -20)                               //> res3: Option[(Double, Double)] = Some((5.0,-2.0))

  //-------------------------------
  // problem 2
  def dist(a: (Int, Int), b: (Int, Int)) = {
    var (x1, y1) = a
    var (x2, y2) = b
    Math.sqrt(Math.pow(x2 - x1, 2) + Math.pow(y2 - y1, 2))

  }                                               //> dist: (a: (Int, Int), b: (Int, Int))Double

  dist((1, 1), (0, 0))                            //> res4: Double = 1.4142135623730951
  dist((3, 0), (0, 0))                            //> res5: Double = 3.0
  dist((1, 2), (3, 4))                            //> res6: Double = 2.8284271247461903

  //-------------------------------
  // problem 3

  def dot(a: (Double, Double, Double), b: (Double, Double, Double)) = {

    var (v1, v2, v3) = a
    var (v4, v5, v6) = b

    var result = ((v1 * v4) + (v2 * v5) + (v3 * v6))

    result
  }                                               //> dot: (a: (Double, Double, Double), b: (Double, Double, Double))Double

  dot((2.0, 3, 4), (2, 2.0, 2))                   //> res7: Double = 18.0
  dot((1, 2, 3), (1, 2, 3))                       //> res8: Double = 14.0
  //-------------------------------
  // problem 6

  def isPrime(n: Int) = {
    var prime = true
    try {
      if (n < 0) throw new Exception("Input must be non-negative")
      else if (n < 2) prime = false
      else if (n == 2) prime = true
      else if (n % 2 == 0) prime = false
      else {
        //if divisible by any number other than self and 1, false
        for (i <- 2 until n - 1) {
          if (n % i == 0) prime = false

        }
      }

    } catch {
      case e: Exception => println(e)
    }
    prime

  }                                               //> isPrime: (n: Int)Boolean

  isPrime(0)                                      //> res9: Boolean = false
  isPrime(1)                                      //> res10: Boolean = false
  isPrime(2)                                      //> res11: Boolean = true
  isPrime(3)                                      //> res12: Boolean = true
  isPrime(7)                                      //> res13: Boolean = true
  isPrime(11)                                     //> res14: Boolean = true
  isPrime(97)                                     //> res15: Boolean = true
  isPrime(101)                                    //> res16: Boolean = true
  isPrime(8)                                      //> res17: Boolean = false
  isPrime(9)                                      //> res18: Boolean = false
  isPrime(33)                                     //> res19: Boolean = false

  isPrime(-1)                                     //> java.lang.Exception: Input must be non-negative
                                                  //| res20: Boolean = true
  //-------------------------------
  // problem 7

  //find greatest common denominator
  def gcd(a: Int, b: Int): Int = {
    if (b == 0)
      a
    else
      gcd(b, a % b)
  }                                               //> gcd: (a: Int, b: Int)Int

  //Euler's phi function
  def phi(n: Int) = {
    var result = 0
    try {
      if (n < 0) throw new Exception("Input must be non-negative")
      else if (n >= 0) {
        for (i <- 0 until Math.abs(n)) {
          if (gcd(i, Math.abs(n)) == 1) {
            result += 1
          }
        }
      }
    } catch {
      case e: Exception => println(e)
    }
    result
  }                                               //> phi: (n: Int)Int
  phi(0)                                          //> res21: Int = 0
  phi(9)                                          //> res22: Int = 6
  phi(10)                                         //> res23: Int = 4
  phi(12)                                         //> res24: Int = 4
  phi(13)                                         //> res25: Int = 12
  phi(15)                                         //> res26: Int = 8
  phi(-1)                                         //> java.lang.Exception: Input must be non-negative
                                                  //| res27: Int = 0

  //-------------------------------
  // problem 8
  def rollDice = {
    var x = Random.nextInt(6) + 1
    var y = Random.nextInt(6) + 1
    (x, y)

  }                                               //> rollDice: => (Int, Int)
  rollDice                                        //> res28: (Int, Int) = (4,6)
  rollDice                                        //> res29: (Int, Int) = (6,4)
  rollDice                                        //> res30: (Int, Int) = (1,1)
  rollDice                                        //> res31: (Int, Int) = (5,6)
  rollDice                                        //> res32: (Int, Int) = (6,2)

}