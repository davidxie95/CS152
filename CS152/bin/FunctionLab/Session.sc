
object Session {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def inc(x: Double) = x + 1                      //> inc: (x: Double)Double
  def double(x: Double) = 2 * x                   //> double: (x: Double)Double

  //-------------------------------
  // problem 1

  def compose[T](f: T => T, g: T => T) = (x: T) => f(g(x))
                                                  //> compose: [T](f: T => T, g: T => T)T => T

  //testers

  val incTWO = compose(inc, inc)                  //> incTWO  : Double => Double = <function1>
  incTWO(1)                                       //> res0: Double = 3.0
  incTWO(2)                                       //> res1: Double = 4.0

  //-------------------------------
  // problem 2

  def selfIter[T](f: T => T, n: Int) = {
    def helper(result: T => T, count: Int): T => T = {
      if (count == 0)
        result
      else helper(compose(f, result), count - 1)
    }

    def id(x: T) = x
    if (n == 0) id(_)
    else
      helper(f, n - 1)
  };                                              //> selfIter: [T](f: T => T, n: Int)T => T

  //testers

  val tester1 = selfIter(inc, 3)                  //> tester1  : Double => Double = <function1>
  tester1(2)                                      //> res2: Double = 5.0
  tester1(3)                                      //> res3: Double = 6.0

  val tester2 = selfIter(double, 3)               //> tester2  : Double => Double = <function1>
  tester2(2)                                      //> res4: Double = 16.0
  tester2(3)                                      //> res5: Double = 24.0

  //-------------------------------
  // problem 3

  def countPass[T](a: Array[T]) = {
    var count = 0
    for (x <- a) {
      if (x == true || x == false)
        count += 1
    }
    count
  }                                               //> countPass: [T](a: Array[T])Int

  //testers

  val list = Array(1, 2, true, false, false)      //> list  : Array[AnyVal] = Array(1, 2, true, false, false)
  val list2 = Array(1, 2, 3, 4, 5, 6)             //> list2  : Array[Int] = Array(1, 2, 3, 4, 5, 6)
  val list3 = Array(1, 2, 3, 4, false)            //> list3  : Array[AnyVal] = Array(1, 2, 3, 4, false)

  countPass(list)                                 //> res6: Int = 3
  countPass(list2)                                //> res7: Int = 0
  countPass(list3)                                //> res8: Int = 1

  //-------------------------------
  // problem 4

  def recur(baseVal: Int, combiner: (Int, Int) => Int): Int => Int = {
    def helper(n: Int): Int = {
      if (n == 0) baseVal
      else combiner(n, helper(n - 1))
    }
    helper
  }                                               //> recur: (baseVal: Int, combiner: (Int, Int) => Int)Int => Int

  val recurFact = recur(1, (x: Int, y: Int) => x * y)
                                                  //> recurFact  : Int => Int = <function1>
  recurFact(5)                                    //> res9: Int = 120
  recurFact(4)                                    //> res10: Int = 24
  recurFact(3)                                    //> res11: Int = 6
  //-------------------------------
  // problem 5
  def parseDigits(digits: String): Option[Int] =
    if (digits.matches("[0-9]*")) Some(digits.toInt) else None
                                                  //> parseDigits: (digits: String)Option[Int]

   def deOptionize[T, S](f: T => Option[S]): T => S = {
     def r(t: T) =
        f(t) match {
           case None => throw new Exception("f failed")
           case Some(s) => s
        }
     r _
  }                                               //> deOptionize: [T, S](f: T => Option[S])T => S
    
  val parseDigits2 = deOptionize(parseDigits _)   //> parseDigits2  : String => Int = <function1>
  
  parseDigits2("2345")                            //> res12: Int = 2345
  
  try {
     parseDigits2("234x5")
  } catch {
     case e: Exception => println(e)
  }                                               //> java.lang.Exception: f failed
                                                  //| res13: AnyVal = ()

}