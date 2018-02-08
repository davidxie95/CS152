package Recursion

object Recursion {
  println("Recursion worksheet: problems 1, 2, 3, 4, 5, 9, 10")
                                                  //> Recursion worksheet: problems 1, 2, 3, 4, 5, 9, 10

  def inc(n: Int) = n + 1                         //> inc: (n: Int)Int
  def dec(n: Int) = n - 1                         //> dec: (n: Int)Int
  def isZero(n: Int) = n == 0                     //> isZero: (n: Int)Boolean

  //-------------------------------
  // problem 1

  def add(n: Int, m: Int): Int = {
    if (isZero(n)) m
    else
      inc(add(dec(n), m))
  }                                               //> add: (n: Int, m: Int)Int

  add(0, 0)                                       //> res0: Int = 0
  add(0, 1)                                       //> res1: Int = 1
  add(1, 0)                                       //> res2: Int = 1
  add(3, 5)                                       //> res3: Int = 8
  add(99, 1)                                      //> res4: Int = 100

  //-------------------------------
  // problem 2

  def mul(n: Int, m: Int): Int = {
    if (isZero(n)) 0
    else
      add(m, mul(dec(n), m))
  }                                               //> mul: (n: Int, m: Int)Int
  mul(0, 0)                                       //> res5: Int = 0
  mul(1, 0)                                       //> res6: Int = 0
  mul(0, 1)                                       //> res7: Int = 0
  mul(2, 2)                                       //> res8: Int = 4
  mul(5, 1)                                       //> res9: Int = 5
  mul(3, 5)                                       //> res10: Int = 15
  mul(99, 1)                                      //> res11: Int = 99

  //-------------------------------
  // problem 3

  def exp2(m: Int): Int = {

    //check for 0
    if (isZero(m)) {
      1
    } else { //if greater than 0
      def helper(result: Int, count: Int): Int =

        if (isZero(count)) {
          result
        } else
          helper(mul(result, 2), dec(count))
      helper(1, m)
    }

  }                                               //> exp2: (m: Int)Int

  exp2(0)                                         //> res12: Int = 1
  exp2(1)                                         //> res13: Int = 2
  exp2(2)                                         //> res14: Int = 4
  exp2(3)                                         //> res15: Int = 8
  exp2(4)                                         //> res16: Int = 16

  //-------------------------------
  // problem 4

  def hyperExp(n: Int): Int = {

    //check for 0
    if (isZero(n)) 1
    else { //if greater than 0
      def helper(result: Int, count: Int): Int =

        if (isZero(count)) {
          result
        } else
          helper(exp2(result), dec(count))

      helper(1, n)
    }
  }                                               //> hyperExp: (n: Int)Int

  hyperExp(0)                                     //> res17: Int = 1
  hyperExp(1)                                     //> res18: Int = 2
  hyperExp(2)                                     //> res19: Int = 4
  hyperExp(3)                                     //> res20: Int = 16

  //-------------------------------
  // problem 5
  // The above definition has been updated to tail recursion

  //-------------------------------
  // problem 9
  def fib(n: Int): Int = {
    if (isZero(n)) 0
    else if (isZero(dec(n))) 1
    else fib(dec(n)) + fib(dec(dec(n)))
  }                                               //> fib: (n: Int)Int
  for (i <- 0 until 10) println(fib(i))           //> 0
                                                  //| 1
                                                  //| 1
                                                  //| 2
                                                  //| 3
                                                  //| 5
                                                  //| 8
                                                  //| 13
                                                  //| 21
                                                  //| 34

  def fib_Tail(n: Int): Int = {

    def helper(count: Int, first: Int, second: Int): Int = {
      if (isZero(count)) first
      else helper(dec(count), second, add(second, first))
    }
    helper(n, 0, 1)

  }                                               //> fib_Tail: (n: Int)Int
  for (i <- 0 until 10) println(fib_Tail(i))      //> 0
                                                  //| 1
                                                  //| 1
                                                  //| 2
                                                  //| 3
                                                  //| 5
                                                  //| 8
                                                  //| 13
                                                  //| 21
                                                  //| 34

  //-------------------------------
  // problem 10

  //assume n is greater than m
  def choose(n: Int, m: Int): Int = {

    //if n is less than m, return 0
    if (isZero(n)) 0

    else if (isZero(m) || m == n) 1
    else choose(dec(n), dec(m)) + choose(dec(n), m)

  }                                               //> choose: (n: Int, m: Int)Int

  choose(1, 2)                                    //> res21: Int = 0
  choose(2, 3)                                    //> res22: Int = 0
  choose(4, 2)                                    //> res23: Int = 6
  choose(6, 4)                                    //> res24: Int = 15
  choose(8, 2)                                    //> res25: Int = 28
}