package ControlLab

object SequenceControl {
  println("Problem 1, Sequence Control ")         //> Problem 1, Sequence Control 

  def calculateTax(income: Double) = {

    var rate = income

    try {

      if (income < 0) {
        throw new Exception("Invalid Income exception, input must be non-negative");

      } else if (income >= 0 && income < 20000) {
        rate = 0.0

      } else if (income >= 20000 && income < 30000) {
        rate = income * 0.05

      } else if (income >= 30000 && income < 40000) {
        rate = income * 0.11

      } else if (income >= 40000 && income < 60000) {
        rate = income * 0.23

      } else if (income >= 60000 && income < 100000) {
        rate = income * 0.32

      } else if (income >= 100000) {
        rate = income * 0.50
      }
      

    } catch {
      case e: Exception => println(e)
    }
    
    rate

  }                                               //> calculateTax: (income: Double)Double

  calculateTax(2)                                 //> res0: Double = 0.0
  calculateTax(25000.00)                          //> res1: Double = 1250.0
  calculateTax(50123)                             //> res2: Double = 11528.29
  calculateTax(-2)                                //> java.lang.Exception: Invalid Income exception, input must be non-negative
                                                  //| res3: Double = -2.0

}