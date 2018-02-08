package List_Processing
import scala.collection.immutable.List

object List_Processing_I {

  //-------------------------------
  // problem 1, 4 implementations

  def isOdd(n: Int) = n % 2 != 0                  //> isOdd: (n: Int)Boolean
  def cube(n: Int) = n * n * n                    //> cube: (n: Int)Int

  //iter
  def sumOddCubeIter(nums: List[Int]): Int = {
    var result = 0
    for (i <- nums if isOdd(i)) result += cube(i)
    result
  }                                               //> sumOddCubeIter: (nums: List[Int])Int

  //recur
  def sumOddCubeRecur(nums: List[Int]): Int = {
    if (nums == Nil) 0
    else if (isOdd(nums.head))
      //add if odd
      cube(nums.head) + sumOddCubeRecur(nums.tail)
    else
      //else go to next number on the list
      sumOddCubeRecur(nums.tail)

  }                                               //> sumOddCubeRecur: (nums: List[Int])Int
  
  //tail
  def sumOddCubeTail(nums: List[Int]): Int = {
    def helper(nums: List[Int], result: Int): Int = {
      if (nums == Nil) result
      else if (isOdd(nums.head))
        //add if odd
        helper(nums.tail, cube(nums.head) + result)
      else
        //else go to next number on the list
        helper(nums.tail, result)
    }
    helper(nums, 0)
  }                                               //> sumOddCubeTail: (nums: List[Int])Int

  //map
  def sumOddCubeMap(nums: List[Int]) = nums.filter(isOdd).map(cube).reduce(_ + _)
                                                  //> sumOddCubeMap: (nums: List[Int])Int
  //testers
  val nums1 = List(1, 2, 3, 4, 5)                 //> nums1  : List[Int] = List(1, 2, 3, 4, 5)
  val nums2 = List(2, 2, 3)                       //> nums2  : List[Int] = List(2, 2, 3)

  sumOddCubeIter(nums1)                           //> res0: Int = 153
  sumOddCubeIter(nums2)                           //> res1: Int = 27

  sumOddCubeRecur(nums1)                          //> res2: Int = 153
  sumOddCubeRecur(nums2)                          //> res3: Int = 27

  sumOddCubeTail(nums1)                           //> res4: Int = 153
  sumOddCubeTail(nums2)                           //> res5: Int = 27

  sumOddCubeMap(nums1)                            //> res6: Int = 153
  sumOddCubeMap(nums2)                            //> res7: Int = 27

  //-------------------------------
  // problem 2, 4 implementations

  //iter
  def sumOfSumsIter(list: List[List[Int]]) = {
    var result = 0
    for (i <- list) {
      for (j <- i) {
        result += j
      }
    }
    result
  }                                               //> sumOfSumsIter: (list: List[List[Int]])Int

  //recur
  def sumOfSumsRecur(list: List[List[Int]]): Int = {
    if (list == Nil) 0
    else list.head.sum + sumOfSumsRecur(list.tail)
  }                                               //> sumOfSumsRecur: (list: List[List[Int]])Int

  //tail
  def sumOfSumsTail(list: List[List[Int]]): Int = {
    def helper(list: List[List[Int]], result: Int): Int = {
      if (list == Nil) result
      else
        helper(list.tail, result + list.head.sum)
    }
    helper(list, 0)
  }                                               //> sumOfSumsTail: (list: List[List[Int]])Int

  //map
  def sumOfSumsMap(list: List[List[Int]]) = {
    def helper(list2: List[Int]) = {
      list2.reduce(_ + _)
    }
    list.map(helper).reduce(_ + _)
  }                                               //> sumOfSumsMap: (list: List[List[Int]])Int

  //testers
  sumOfSumsIter(List(List(1, 2, 3), List(4, 5, 6)))
                                                  //> res8: Int = 21
  sumOfSumsRecur(List(List(1, 2, 3), List(4, 5, 6)))
                                                  //> res9: Int = 21
  sumOfSumsTail(List(List(1, 2, 3), List(4, 5, 6)))
                                                  //> res10: Int = 21
  sumOfSumsMap(List(List(1, 2, 3), List(4, 5, 6)))//> res11: Int = 21

  //-------------------------------
  // problem 3

  def depth(vals: Any): Int =
    vals match {
      case (first :: rest) => math.max(depth(first) + 1, depth(rest))
      case _               => 0
    }                                             //> depth: (vals: Any)Int

  depth(List(List(List(1, 2, List(3)))))          //> res12: Int = 4

  //-------------------------------
  // problem 6, 4 implementations

  //iter
  def predicateIter[T](pred: T => Boolean, list: List[T]): Int = {
    var result = 0
    for (i <- list)
      if (pred(i))
        result += 1
    result
  }                                               //> predicateIter: [T](pred: T => Boolean, list: List[T])Int

  //recur
  def predicateRecur[T](pred: T => Boolean, list: List[T]): Int = {
    if (list == Nil) 0
    else if (pred(list.head))
      predicateRecur(pred, list.tail) + 1
    else
      predicateRecur(pred, list.tail)
  }                                               //> predicateRecur: [T](pred: T => Boolean, list: List[T])Int

  //tail
  def predicateTail[T](pred: T => Boolean, list: List[T]): Int = {
    def helper(pred: T => Boolean, list: List[T], result: Int): Int = {
      if (list == Nil) result
      else if (pred(list.head))
        helper(pred, list.tail, result + 1)
      else
        helper(pred, list.tail, result)
    }
    helper(pred, list, 0)
  }                                               //> predicateTail: [T](pred: T => Boolean, list: List[T])Int

  //map
  def predicateMap[T](pred: T => Boolean, list: List[T]): Int = {
    list.filter(pred).length
  }                                               //> predicateMap: [T](pred: T => Boolean, list: List[T])Int

  //testers
  val list = List(1, 2, 3, 4, 5)                  //> list  : List[Int] = List(1, 2, 3, 4, 5)
  val list2 = List(6, 7, 8, 9, 10)                //> list2  : List[Int] = List(6, 7, 8, 9, 10)

  predicateIter(isOdd, list)                      //> res13: Int = 3
  predicateIter(isOdd, list2)                     //> res14: Int = 2

  predicateRecur(isOdd, list)                     //> res15: Int = 3
  predicateRecur(isOdd, list2)                    //> res16: Int = 2

  predicateTail(isOdd, list)                      //> res17: Int = 3
  predicateTail(isOdd, list2)                     //> res18: Int = 2

  predicateMap(isOdd, list)                       //> res19: Int = 3
  predicateMap(isOdd, list2)                      //> res20: Int = 2

  //-------------------------------
  // problem 7, 4 implementations

  //iter
  def iterAll[T](pred: T => Boolean, list: List[T]): Boolean = {
    var result = true
    for (i <- list if result) {
      result = result && pred(i)
    }
    result
  }                                               //> iterAll: [T](pred: T => Boolean, list: List[T])Boolean

  //recur
  def recurAll[T](pred: T => Boolean, list: List[T]): Boolean =
    if (list == Nil) true
    else if (pred(list.head))
      recurAll(pred, list.tail)
    else false                                    //> recurAll: [T](pred: T => Boolean, list: List[T])Boolean

  //tail
  def tailAll[T](pred: T => Boolean, list: List[T]): Boolean = {
    def helper[T](pred: T => Boolean, list: List[T], result: Boolean): Boolean = {
      if (list == Nil) result
      else if (pred(list.head))
        helper(pred, list.tail, result)
      else false
    }
    helper(pred, list, true)
  }                                               //> tailAll: [T](pred: T => Boolean, list: List[T])Boolean

  //map
  def mapAll[T](pred: T => Boolean, list: List[T]): Boolean = {
    list.filter(pred).length == list.length
  }                                               //> mapAll: [T](pred: T => Boolean, list: List[T])Boolean

  //testers
  def isEven(n: Int) = n % 2 == 0                 //> isEven: (n: Int)Boolean
  def isPal(s: String) = s == s.reverse           //> isPal: (s: String)Boolean

  val test0 = List(2, 4, 6, 8, 10)                //> test0  : List[Int] = List(2, 4, 6, 8, 10)
  val test1 = List(1, 2, 3, 4, 5)                 //> test1  : List[Int] = List(1, 2, 3, 4, 5)
  val test2 = List("mom", "rotator", "dad")       //> test2  : List[String] = List(mom, rotator, dad)

  iterAll(isEven, test0)                          //> res21: Boolean = true
  iterAll(isEven, test1)                          //> res22: Boolean = false
  iterAll(isPal, test2)                           //> res23: Boolean = true

  recurAll(isEven, test0)                         //> res24: Boolean = true
  recurAll(isEven, test1)                         //> res25: Boolean = false
  recurAll(isPal, test2)                          //> res26: Boolean = true

  tailAll(isEven, test0)                          //> res27: Boolean = true
  tailAll(isEven, test1)                          //> res28: Boolean = false
  tailAll(isPal, test2)                           //> res29: Boolean = true

  mapAll(isEven, test0)                           //> res30: Boolean = true
  mapAll(isEven, test1)                           //> res31: Boolean = false
  mapAll(isPal, test2)                            //> res32: Boolean = true

  //-------------------------------
  // problem 8, 4 implementations

  //iter
  def iterAny[T](pred: T => Boolean, list: List[T]): Boolean = {
    var result = false
    for (i <- list)
      if (pred(i))
        result = true
    result
  }                                               //> iterAny: [T](pred: T => Boolean, list: List[T])Boolean

  //recur
  def recurAny[T](pred: T => Boolean, list: List[T]): Boolean = {
    if (list == Nil) false
    else if (pred(list.head)) true
    else recurAny(pred, list.tail)
  }                                               //> recurAny: [T](pred: T => Boolean, list: List[T])Boolean

  //tail
  def tailAny[T](pred: T => Boolean, list: List[T]): Boolean = {
    def helper[T](pred: T => Boolean, list: List[T], result: Boolean): Boolean = {
      if (list == Nil) result
      else if (pred(list.head)) true
      else helper(pred, list.tail, result)
    }
    helper(pred, list, false)
  }                                               //> tailAny: [T](pred: T => Boolean, list: List[T])Boolean

  //map
  def mapAny[T](pred: T => Boolean, list: List[T]): Boolean = {
    list.filter(pred).length > 0
  }                                               //> mapAny: [T](pred: T => Boolean, list: List[T])Boolean

  //testers
  val someOddList = List(1, 2, 3, 4, 5)           //> someOddList  : List[Int] = List(1, 2, 3, 4, 5)
  val evenList = List(2, 4, 6, 8, 10)             //> evenList  : List[Int] = List(2, 4, 6, 8, 10)

  iterAny(isOdd, someOddList)                     //> res33: Boolean = true
  iterAny(isOdd, evenList)                        //> res34: Boolean = false

  recurAny(isOdd, someOddList)                    //> res35: Boolean = true
  recurAny(isOdd, evenList)                       //> res36: Boolean = false

  tailAny(isOdd, someOddList)                     //> res37: Boolean = true
  tailAny(isOdd, evenList)                        //> res38: Boolean = false

  mapAny(isOdd, someOddList)                      //> res39: Boolean = true
  mapAny(isOdd, evenList)                         //> res40: Boolean = false

  //-------------------------------
  // problem 10

  def isSorted(list: List[Int]): Boolean = {
    list == list.sorted
  }                                               //> isSorted: (list: List[Int])Boolean

  val sortedList = List(1, 2, 3, 4, 5)            //> sortedList  : List[Int] = List(1, 2, 3, 4, 5)
  val unSortedList = List(4, 1, 0, 9, 8, 11)      //> unSortedList  : List[Int] = List(4, 1, 0, 9, 8, 11)
  
  isSorted(sortedList)                            //> res41: Boolean = true
  isSorted(unSortedList)                          //> res42: Boolean = false

  //-------------------------------
  // problem 13
  
  def isPositive(n: Int) = n > 0                  //> isPositive: (n: Int)Boolean
  isPositive(1)                                   //> res43: Boolean = true
  isPositive(-1)                                  //> res44: Boolean = false

  def isOne(n: Int) = n == 1                      //> isOne: (n: Int)Boolean
  isOne(1)                                        //> res45: Boolean = true
  isOne(2)                                        //> res46: Boolean = false
  
  //ones
  def makeOnes(from: Int): Stream[Int] = 1 #:: makeOnes(1)
                                                  //> makeOnes: (from: Int)Stream[Int]
  val nat = makeOnes(0)                           //> nat  : Stream[Int] = Stream(1, ?)
  val ones = nat.filter(isOne)                    //> ones  : scala.collection.immutable.Stream[Int] = Stream(1, ?)
  ones(5)                                         //> res47: Int = 1
  
  //non-negatives
  def makeNats(from: Int): Stream[Int] = from #:: makeNats(from + 1)
                                                  //> makeNats: (from: Int)Stream[Int]
  val nats = makeNats(0)                          //> nats  : Stream[Int] = Stream(0, ?)

  val nonNegative = nats.filter(isPositive)       //> nonNegative  : scala.collection.immutable.Stream[Int] = Stream(1, ?)
  nonNegative(5)                                  //> res48: Int = 6

	//even
  val evenIntegers = nats.filter(isEven)          //> evenIntegers  : scala.collection.immutable.Stream[Int] = Stream(0, ?)
  evenIntegers(5)                                 //> res49: Int = 10

	//square
  val sqaureIntegers = nonNegative.map((x: Int) => x * x)
                                                  //> sqaureIntegers  : scala.collection.immutable.Stream[Int] = Stream(1, ?)
  sqaureIntegers(5)                               //> res50: Int = 36



}