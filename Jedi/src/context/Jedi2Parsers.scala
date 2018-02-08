package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi2Parsers extends Jedi1Parsers {

  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  def params: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case None => Nil
    case Some(id ~ Nil) => List(id)
    case Some(id ~ more) => id :: more
  }

  // lambda parser
  def lambda: Parser[Lambda] = "lambda" ~ params ~ expression ^^ {
    case "lambda" ~ p ~ e => Lambda(p, e)
  }

  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  def block: Parser[Expression] = "{" ~> expression ~ rep(";" ~> expression) <~ "}" ^^ {
    case e ~ Nil => Block(List(e))
    case e ~ more => Block(e :: more)
  }

  // override of term parser
  override def term: Parser[Expression] = lambda | funCall | block | literal | "(" ~> expression <~ ")"
}
