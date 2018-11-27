package edu.luc.cs.laufer.cs473.expressions

import scala.util.parsing.combinator.JavaTokenParsers
import ast._

object CombinatorParser extends JavaTokenParsers {

  /** expr ::= term { { "+" | "-" } term }* */
  def expr: Parser[Expr] =
    term ~! rep(("+" | "-") ~ term) ^^ {
      case l ~ exprs => exprs.foldLeft(l) {
        case (e, "+" ~ r) => Plus(e, r)
        case (e, "-" ~ r) => Minus(e, r)
      }
    }

  /** term ::= factor { { "*" | "/" | "%" } factor }* */
  def term: Parser[Expr] =
    factor ~! rep(("*" | "/" | "%") ~ term) ^^ {
      case l ~ terms => terms.foldLeft(l) {
        case (e, "*" ~ r) => Times(e, r)
        case (e, "/" ~ r) => Div(e, r)
        case (e, "%" ~ r) => Mod(e, r)
      }
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

  /**
    * add support for these other constructs
    * statement   ::= expression ";" | assignment | conditional | loop | block
    * assignment  ::= ident "=" expression ";"
    * conditional ::= "if" "(" expression ")" block [ "else" block ]
    * loop        ::= "while" "(" expression ")" block
    * block       ::= "{" statement* "}"
    */
  def statement: Parser[Expr] = (
    expr ~ ";" ^^ { case e ~ _ => e } //expression case
    | rep1sep(ident, ".") ~ "=" ~ expr ~ ";" ^^ { case v ~ _ ~ e ~ _ => Assign(v, e) } //assignment case
    | conditional
    | loop
    | block
  )

  def conditional: Parser[Expr] = (
    "if" ~ "(" ~ expr ~ ")" ~ block ~ opt("else" ~ block) ^^ {
      case _ ~ _ ~ e ~ _ ~ tb ~ Some(_ ~ eb) => Cond(e, tb, eb)
      case _ ~ _ ~ e ~ _ ~ tb ~ None         => Cond(e, tb, Block())
    }
  )

  def loop: Parser[Expr] = (
    "while" ~ "(" ~ expr ~ ")" ~ block ^^ { case _ ~ _ ~ e ~ _ ~ b => Loop(e, b) }
  )

  def block: Parser[Expr] = (
    "{" ~ rep(statement) ~ "}" ^^ { case _ ~ statements ~ _ => Block(statements: _*) }
  )

  def topLevel: Parser[Expr] = (
    rep1(statement) ^^ { case exprs => Block(exprs: _*) }
  )
}
