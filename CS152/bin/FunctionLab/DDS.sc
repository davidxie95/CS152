
object DDS {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  //-------------------------------
  // problem 1
  def controlLoop[S](state: S, cycle: Int, halt: (S, Int) => Boolean, update: (S, Int) => S): S =
    if (halt(state, cycle)) state
    else controlLoop(update(state, cycle), cycle + 1, halt, update)
                                                  //> controlLoop: [S](state: S, cycle: Int, halt: (S, Int) => Boolean, update: (S
                                                  //| , Int) => S)S
  //-------------------------------
  // problem 2
  val finalPop = controlLoop(1, 0, (p: Int, t: Int) => p >= 10000, (p: Int, t: Int) => 2 * p)
                                                  //> finalPop  : Int = 16384
  finalPop                                        //> res0: Int = 16384

  //-------------------------------
  // problem 3

  val delta = 1e-5                                //> delta  : Double = 1.0E-5

  def deriv(f: Double => Double): Double => Double = {
    def df(x: Double) = (f(x + delta) - f(x)) / delta
    df _
  }                                               //> deriv: (f: Double => Double)Double => Double
  def foo(x: Double) = 3 * x * x                  //> foo: (x: Double)Double
  val dfoo = deriv(foo)                           //> dfoo  : Double => Double = <function1>
  dfoo(10)                                        //> res1: Double = 60.00002999826392

  def solve(f: Double => Double): Double = {
    def df = deriv(f)
    def goodEnuf(guess: Double, c: Int) = math.abs(f(guess)) <= delta
    def improve(guess: Double, c: Int) = guess - f(guess) / df(guess)
    controlLoop(1.0, 0, goodEnuf, improve)
  }                                               //> solve: (f: Double => Double)Double
  //-------------------------------
  // problem 4
  def squareRoot(n: Double) = solve((x: Double) => x * x - n)
                                                  //> squareRoot: (n: Double)Double

  squareRoot(81)                                  //> res2: Double = 9.000000000013383
  squareRoot(49)                                  //> res3: Double = 7.000000142285558
  squareRoot(100)                                 //> res4: Double = 10.000000000166429
  //-------------------------------
  // problem 5
  def cubeRoot(n: Double) = solve((x: Double) => x * x * x - n)
                                                  //> cubeRoot: (n: Double)Double

  cubeRoot(8)                                     //> res5: Double = 2.000000000036784
  cubeRoot(27)                                    //> res6: Double = 3.0000000000019176
  cubeRoot(64)                                    //> res7: Double = 4.000000000119973

  //-------------------------------
  // problem 6
  def nthRoot(n: Double, m: Int) = {
    def h(x: Double) = math.pow(x, m) - n
    solve(h)
  }                                               //> nthRoot: (n: Double, m: Int)Double

  nthRoot(4, 2)                                   //> res8: Double = 2.0000000944796694
  nthRoot(9, 2)                                   //> res9: Double = 3.0000000015508212
  nthRoot(16, 2)                                  //> res10: Double = 4.000000639575587
  nthRoot(125, 3)                                 //> res11: Double = 5.000000000364238
}