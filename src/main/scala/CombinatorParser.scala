package edu.luc.cs.laufer.cs473.expressions

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import ast._

object CombinatorParser extends StandardTokenParsers {

  lexical.delimiters += ("(", ")", "+", "-", "*", "/", "%")

  /** expr ::= term { { "+" | "-" } term }* */
  def expr: Parser[Expr] =
    term ~! opt(("+" | "-") ~ term) ^^ {
      case l ~ None => l
      case l ~ Some("+" ~ r) => Plus(l, r)
      case l ~ Some("-" ~ r) => Minus(l, r)
    }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Expr] =
    factor ~! opt(("*" | "/" | "%") ~ factor) ^^ {
      case l ~ None => l
      case l ~ Some("*" ~ r) => Times(l, r)
      case l ~ Some("/" ~ r) => Div(l, r)
      case l ~ Some("%" ~ r) => Mod(l, r)
    }

  /** factor ::= ident | numericLit | "+" factor | "-" factor | "(" expr ")" */
  def factor: Parser[Expr] = (
    ident ^^ { case i => Identifier(i)}
  |  numericLit ^^ { case s => Constant(s.toInt) }
  | "+" ~> factor ^^ { case e => e }
  | "-" ~> factor ^^ { case e => UMinus(e) }
  | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
  )

  /** statement   ::= expression ";" | assignment | conditional | loop | block */
  def statement: Parser[Expr] = (
    expr <~ ";" ^^ { case e => e }
    | assignment ^^ { case a => a }
    | conditional ^^ { case c => c}
    | loop ^^ { case l => l}
    | block ^^ { case b => b }
    )

  /** assignment  ::= ident "=" expression ";" */
  def assignment: Parser[Expr] =
    ident ~ "=" ~ expr ~ ";" ^^ {
      case i ~ "=" ~ e ~ ";" => Assignment(Identifier(i), e)
    }


  /** conditional ::= "if" "(" expression ")" block [ "else" block ] */
  def conditional: Parser[Expr] =
    "if" ~ "{" ~ expr ~ "}" ~ block ~ opt( "else" ~ block ) ^^ {
      case _ ~ _ ~ guard ~ _ ~ ifBranch ~ None => Conditional(guard, ifBranch)
      case _ ~ _ ~ guard ~ _ ~ ifBranch ~ Some(_ ~ elseBranch) => Conditional(guard, ifBranch, Some(elseBranch))
    }

  /** loop ::= "while" "(" expression ")" block */
  def loop: Parser[Expr] =
    "while" ~ "(" ~ expr ~ ")" ~ block ^^ {
      case _ ~ _ ~ guard ~ _ ~ body => Loop(guard, body)
    }

  /** block ::= "{" statement* "}" */
  def block: Parser[Expr] =
    "{" ~ rep(statement) ~ "}" ^^ {
      case _ ~ statements ~ _ => Block(statements:_*)
    }

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] =
    phrase(p)(new lexical.Scanner(in))
}
