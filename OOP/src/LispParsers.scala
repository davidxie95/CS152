//package littleLisp
import scala.io._
import scala.util.parsing.combinator._

/**
 * * LL Test result ***
 * Please copy paste a sample session of your Little Lisp interpreter into your solution sheet. Include executions of the following expressions:
 *
 * -3.14
 * +42
 * (+ 2 3 4.1)
 * (* 2.3 3 4)
 * (* (* 2 3) (+ 9 -2))
 * (+ 3 4 two)
 *
 * The last one should produce an error.
 *
 *
 * *
 */

/*** LL expressions ***/
trait Expression {
  def execute: Number
}

/*** LL numbers ***/

// note: also needs toString
case class Number(val value: Double) extends Expression {

  def execute = new Number(value)
  
  def +(other: Number): Number = new Number(value + other.value)
  def -(other: Number): Number = new Number(value - other.value)
  def *(other: Number): Number = new Number(value * other.value)

  override def toString = "" + value + ""

}

/*** LL function calls ***/

// note: for full credit use match, map and reduce in execute
case class FunCall(val operator: String, op1: Number, op2: Number) extends Expression {
  
  def execute = {
    if (operator == "+")
      op1+op2
    else if (operator == "*")
      op1*op2
    else
      op1-op2
    
  }
}

/*** LL parsers ***/
class LispParsers extends RegexParsers {

  def expression: Parser[Expression] = number | funcall | failure("Invalid expression")

  // number ::= (\+|-)?[0-9]+(\.[0-9]+)?
  def number: Parser[Number] = """(\+|-)?[0-9]+(\.[0-9]+)?""".r ^^ {
    case n => new Number(n.toDouble)
  }

  // operator ::= + |*
  def operator: Parser[String] = "+"

  // funcall ::= "(" ~ operator ~ expression* ~ ")" 
  def funcall: Parser[FunCall] = "(" ~ operator ~> expression ~ ")" ^^ {

    case n => new FunCall(n.toString, Number(3.0), Number(1.0))

  }
}
/*** LL console ***/

// note: this is complete

object console {

  val parsers = new LispParsers

  def execute(cmmd: String): String = {
    val tree = parsers.parseAll(parsers.expression, cmmd)
    tree match {
      case tree: parsers.Failure => throw new Exception("syntax error: " + tree)
      case _ => {
        val exp = tree.get // get the expression from the tree
        val result = exp.execute // execute the expression
        result.toString // return string representation of result
      }
    }
  }
  def repl {
    var more = true
    var cmmd = ""
    while (more) {
      try {
        print("-> ")
        cmmd = StdIn.readLine
        if (cmmd == "quit") more = false
        else println(execute(cmmd))
      } catch {
        case e: Exception => println(e.getMessage)
      }
    } // while
    println("bye")
  } // repl   
} // console

/*** LL main ***/

// note: this is complete

object LittleLisp {
  def main(args: Array[String]): Unit = {
    console.repl
  }
}