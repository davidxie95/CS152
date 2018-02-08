package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi3Parsers extends Jedi2Parsers {

  // assignment ::= identifier ~ "=" ~ expression
  def assignment: Parser[Assignment] = identifier ~ "=" ~ expression ^^ {
    case variable ~ "=" ~ update => Assignment(variable, update)
  }

  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
  def iteration: Parser[Iteration] = "while" ~ "(" ~ expression ~ ")" ~ expression ^^ {
    case "while" ~ "(" ~ condition ~ ")" ~ body => Iteration(condition, body)
  }

  // dereference ::= "[" ~ expression ~ "]"
  def dereference: Parser[Expression] = "[" ~ expression ~ "]" ^^ {
    case "[" ~ exp ~ "]" => FunCall(Identifier("dereference"), List(exp))
  }

  //	SWITCH ::= switch~(~EXPRESSION~)~{~EXPRESSION~(;~EXPRESSION)*~}
  def switch: Parser[Expression] = "switch" ~ "(" ~ expression ~ ")" ~ "{" ~> (expression ~ rep(";" ~> expression)) <~ "}" ^^ {
    case exp ~ Nil     => Switch(exp, Nil)
    case exp ~ expList => Switch(exp, expList)
  }
  
  // DO
  def DO: Parser[Expression] = "do"~expression~"while"~expression ^^ {
      case "do"~body~"while"~condition => Iteration(condition, body)
   }
  
  val thunkBody =
    "{def cache = var(0); def cacheReady = var(false); lambda() { if(not([cacheReady])) { cache = EXP; cacheReady = true }; [cache]}}"

  val body = """EXP""".r
  
  //access
  def access: Parser[Expression] = term ~ opt("." ~> identifier) ^^{
      case t ~ None => t
      case t ~ Some(e) => Access(t, e)
    }
  
  
  //TBE

  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")
  override def term: Parser[Expression] = lambda | funCall | block | assignment | dereference | literal | "(" ~> expression <~ ")"
}