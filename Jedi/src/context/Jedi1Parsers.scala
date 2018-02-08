package context

import scala.util.parsing.combinator._
import expression._
import value._

/*
 * Notes:
 * disjunction reduces to conjunction reduces to equality ... reduces to term
 * if A reduces to B, then B will have higher precedence than A
 * Example: sum reduces to product, so a + b * c = a + (b * c)
 * Had to make some big corrections to numeral regex
 * This could probably have been a singleton
 */

class Jedi1Parsers extends RegexParsers {

  def expression: Parser[Expression] = declaration | conditional | disjunction | failure("Invalid expression")

  def declaration: Parser[Declaration] = "def" ~ identifier ~ "=" ~ expression ^^ {
    case "def" ~ id ~ "=" ~ exp => Declaration(id, exp)
  }

  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^ {
    case "if" ~ "(" ~ cond ~ ")" ~ cons ~ None               => Conditional(cond, cons)
    case "if" ~ "(" ~ cond ~ ")" ~ cons ~ Some("else" ~ alt) => Conditional(cond, cons, alt)
  }

  def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^ {
    case con ~ Nil  => con
    case con ~ more => Disjunction(con :: more)
  }

  // conjunction ::= equality ~ ("&&" ~ equality)*
  def conjunction: Parser[Expression] =
    equality ~ rep("&&" ~> equality) ^^ {
      case exp ~ Nil  => exp
      case exp ~ exp2 => Conjunction(exp :: exp2)
    }

  // equality ::= inequality ~ ("==" ~ inequality)*
  def equality: Parser[Expression] =
    inequality ~ rep("==" ~> inequality) ^^ {
      case exp ~ Nil  => exp
      case exp ~ exp2 => FunCall(Identifier("equals"), exp :: exp2)
    }

  // inequality hard coded for now
  
  // inequality ::= sum ~ (("<" | ">" | "!=") ~ sum)?
    def inequality: Parser[Expression] = sum ~ opt(("<" | ">" | "!=") ~ sum) ^^ {
    case t~None=> t
    case t~Some("<" ~ s)=> FunCall(Identifier("less"), List(t, s))
    case t~Some(">" ~ s)=> FunCall(Identifier("more"), List(t, s))
    case t~Some("!=" ~ s)=> FunCall(Identifier("unequals"), List(t, s))
  }

  // negate(exp) = 0 - exp
  private def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Integer(0)
    FunCall(sub, List(zero, exp))
  }

  // sum ::= product ~ ("+" | "-") ~ product)*  
  def sum: Parser[Expression] = product ~ rep(("+" | "-") ~ product ^^ {
    case "+" ~ s => s
    case "-" ~ s => negate(s)
  }) ^^ {
    case p ~ Nil  => p
    case p ~ rest => FunCall(Identifier("add"), p :: rest)
  }

  def invert(exp: Expression): Expression = {
    val div = Identifier("div")
    val one = new Real(1)
    FunCall(div, List(one, exp))
  }

  // product ::= term ~ (("*" | "/") ~ term)*
  def product: Parser[Expression] = term ~ rep(("*" | "/") ~ term ^^ {
    case "*" ~ s => s
    case "/" ~ s => invert(s)
  }) ^^
    {
      case p ~ Nil  => p
      case p ~ rest => FunCall(Identifier("mul"), p :: rest)
    }

  def term: Parser[Expression] = funCall | literal | "(" ~> expression <~ ")"

  def literal = boole | real | integer | text | identifier

  // text ::= any chars bracketed by quotes
  def text: Parser[Text] = """\"[^"]+\"""".r ^^ {
    case chars => Text(chars.substring(1, chars.length - 1))
  }

  // integer ::= 0|(\+|-)?[1-9][0-9]*
  def integer: Parser[Integer] = """0|(\+|-)?[1-9][0-9]*""".r ^^ {
    case n => new Integer(n.toInt)
  }

  // real ::= (\+|-)?[0-9]+\.[0-9]+
  def real: Parser[Real] = """(\+|-)?[0-9]+\.[0-9]+""".r ^^ {
    case n => new Real(n.toDouble)
  }

  // boole ::= true | false
  def boole: Parser[Boole] = ("true" | "false") ^^ {
    case "true"  => new Boole(true)
    case "false" => new Boole(false)
  }

  // identifier ::= [a-zA-Z][a-zA-Z0-9]*
  def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ {
    case t => Identifier(t.toString)
  }

   def operands: Parser[List[Expression]] = "("~opt(expression ~ rep("," ~> expression))~")" ^^ {
     case "("~None~")" => Nil
     case "(" ~ Some(e~exps) ~ ")" => e::exps
   }
   
   def funCall: Parser[Expression] = identifier~operands ^^ {
     case op~ops => FunCall(op, ops)
   }
}