package ControlLab

import scala.util.Random

object StringProcessing {

  //-------------------------------
  // problem 1

  def isPal(s: String) = {

    var returnVal = true

    for (x <- 0 until s.length / 2) {
      if (s.substring(x, x + 1) != s.substring((s.length - x - 1), (s.length - x))) {

        returnVal = false
      }

    }
    returnVal

  }                                               //> isPal: (s: String)Boolean

  isPal("rotator")                                //> res0: Boolean = true
  isPal("cat")                                    //> res1: Boolean = false
  isPal("11a11")                                  //> res2: Boolean = true
  isPal("%^^%")                                   //> res3: Boolean = true
  isPal("12345")                                  //> res4: Boolean = false

  //-------------------------------
  // problem 2

  def isPal2(s: String) = {

    var returnVal = true
    var temp = s.replaceAll("\\s|\\,|\\!|\\.|\\?|\\&|\\%|\\-|\\/", "").toLowerCase

    for (x <- 0 until temp.length / 2) {
      if (temp.substring(x, x + 1) != temp.substring((temp.length - x - 1), (temp.length - x))) {

        returnVal = false
      }

    }
    returnVal
  }                                               //> isPal2: (s: String)Boolean

  isPal2("rotator- %! . ,  /")                    //> res5: Boolean = true
  isPal2("cat")                                   //> res6: Boolean = false
  isPal2("A man, a plan, a canal, Panama!")       //> res7: Boolean = true

  //-------------------------------
  // problem 4
  def mkWord(length: Int = 11) = {

    var returnVal = ""
    val alpha = "abcdefghijklmnopqrstuvwxyz"
    val r = new scala.util.Random()
    var temp = ""

    for (i <- 0 until length) {

      var sub = r.nextInt(25)
      temp = alpha.substring(sub, sub + 1)
      returnVal += temp
    }
    returnVal

  }                                               //> mkWord: (length: Int)String

  val a1 = mkWord()                               //> a1  : String = gvgrkarghgl
  val a2 = mkWord()                               //> a2  : String = lccyeweiisr
  val a3 = mkWord()                               //> a3  : String = okdmubpeywn
  val a4 = mkWord(20)                             //> a4  : String = yjqmohqjwlhvdryjsqpv

  //-------------------------------
  // problem 5
  def mkSentence(length: Int = 8) = {

    var returnVal = ""
    val alpha = "abcdefghijklmnopqrstuvwxyz"
    val r = new scala.util.Random()
    var temp = ""
    for (i <- 0 until length) {
      var size = r.nextInt(length) + 1
      for (i <- 0 until size) {

        var sub = r.nextInt(25)
        temp = alpha.substring(sub, sub + 1)
        returnVal += temp
      }
      if (i != length - 1)
        returnVal += " "
      else
        returnVal += "."
    }

    returnVal = returnVal.capitalize

    returnVal

  }                                               //> mkSentence: (length: Int)String

  val sen1 = mkSentence()                         //> sen1  : String = Tdbrende xktmeini i abtggbi anblivtd lrwamgl bvsfmi horuyb
                                                  //| .
  val sen2 = mkSentence()                         //> sen2  : String = Gukoakwm vwsvyc nbmjo husiqglo tu hbl irjbytj gc.
  val sen3 = mkSentence()                         //> sen3  : String = E awyhwmu jomh impkh eramadkx rh wdspmhp w.
  val sen4 = mkSentence(5)                        //> sen4  : String = Tnl jww qkd be q.
  
  //-------------------------------
  // problem 8
  def add(s: String) ={
    var returnVal = ""
    val temp = s.trim

    var first = ""
    var second = ""

    if (temp.contains("+") == false) {
      returnVal = "Error: Missing Operator"
    } //get first and second from string
    else {
      for (i <- 0 until s.length) {
        if (s.substring(i, i + 1) == "+") {
          var count = i
          try {
            first = s.substring(0, count)
            second = s.substring(count + 1, s.length)
            returnVal = (first.toDouble + second.toDouble).toString
          } catch {
            case e: NumberFormatException => print("Error: Number Format Exception")
          }
         
        }
      }
    }
    returnVal
  }                                               //> add: (s: String)String
  
  add("21 * 43")                                  //> res8: String = Error: Missing Operator
  add("abc + 3")                                  //> Error: Number Format Exceptionres9: String = ""
  add("3.14+42")                                  //> res10: String = 45.14
  add("  -26  +  -49.99  ")                       //> res11: String = -75.99000000000001

  //-------------------------------
  // problem 9
  def eval(s: String) ={
    var returnVal = ""
    val temp = s.trim

    var first = ""
    var second = ""

    if (temp.contains("+") == false && temp.contains("*") == false ) {
      returnVal = "Error: Missing Operator"
    } //get first and second from string
    else {
      for (i <- 0 until s.length) {
        if (s.substring(i, i + 1) == "+") {
          var count = i
          try {
            first = s.substring(0, count)
            second = s.substring(count + 1, s.length)
            returnVal = (first.toDouble + second.toDouble).toString
          } catch {
            case e: NumberFormatException => print("Error: Number Format Exception")
          }
         
        }else if (s.substring(i, i + 1) == "*") {
          var count = i
          try {
            first = s.substring(0, count)
            second = s.substring(count + 1, s.length)
            returnVal = (first.toDouble * second.toDouble).toString
          } catch {
            case e: NumberFormatException => print("Error: Number Format Exception")
          }
         
        }
      }
    }
    returnVal
  }                                               //> eval: (s: String)String

  eval("3.14+42")                                 //> res12: String = 45.14
  eval("  -26  +  -49.99  ")                      //> res13: String = -75.99000000000001
  eval("21 * 43")                                 //> res14: String = 903.0
  eval("abc + 3")                                 //> Error: Number Format Exceptionres15: String = ""


}