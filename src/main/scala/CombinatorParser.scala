package edu.luc.cs.laufer.cs473.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import ast._

object CombinatorParser extends JavaTokenParsers {

  /** expr ::= term { { "+" | "-" } term }* */
  def expr: Parser[Expr] =
    term ~! opt(("+" | "-") ~ term) ^^ {
      case l ~ None          => l
      case l ~ Some("+" ~ r) => Plus(l, r)
      case l ~ Some("-" ~ r) => Minus(l, r)
    }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Expr] =
    factor ~! opt(("*" | "/" | "%") ~ factor) ^^ {
      case l ~ None          => l
      case l ~ Some("*" ~ r) => Times(l, r)
      case l ~ Some("/" ~ r) => Div(l, r)
      case l ~ Some("%" ~ r) => Mod(l, r)
    }

  /** factor ::= ident | ... where ident ::= [a-zA-Z] [a-zA-Z0-9]* */
  /** factor ::= wholeNumber | "+" factor | "-" factor | "(" expr ")" */
  def factor: Parser[Expr] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
    | "+" ~> factor ^^ { case e => e }
    | "-" ~> factor ^^ { case e => UMinus(e) }
    | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
    | ident ^^ { case s => Variable(s) }
  )

  /** add support for these other constructs
    * statement   ::= expression ";" | assignment | conditional | loop | block
    * assignment  ::= ident "=" expression ";"
    * conditional ::= "if" "(" expression ")" block [ "else" block ]
    * loop        ::= "while" "(" expression ")" block
    * block       ::= "{" statement* "}"
    */
  def statement: Parser[Expr] = (
    expr ~ ";" ^^ { case e ~ _ => e }
    | ident ~ "=" ~ expr ~ ";" ^^ { case v ~ _ ~ e ~ _ => Assign(v, e) }
  )
}
