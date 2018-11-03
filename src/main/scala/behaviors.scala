package edu.luc.cs.laufer.cs473.expressions

import ast._

object behaviors {

  def evaluate(e: Expr): Int = e match {
    case Constant(c) => c
    case UMinus(r)   => -evaluate(r)
    case Plus(l, r)  => evaluate(l) + evaluate(r)
    case Minus(l, r) => evaluate(l) - evaluate(r)
    case Times(l, r) => evaluate(l) * evaluate(r)
    case Div(l, r)   => evaluate(l) / evaluate(r)
    case Mod(l, r)   => evaluate(l) % evaluate(r)
  }

  def size(e: Expr): Int = e match {
    case Constant(c) => 1
    case UMinus(r)   => 1 + size(r)
    case Plus(l, r)  => 1 + size(l) + size(r)
    case Minus(l, r) => 1 + size(l) + size(r)
    case Times(l, r) => 1 + size(l) + size(r)
    case Div(l, r)   => 1 + size(l) + size(r)
    case Mod(l, r)   => 1 + size(l) + size(r)
    case Variable(n) => 1
  }

  def height(e: Expr): Int = e match {
    case Constant(c) => 1
    case UMinus(r)   => 1 + height(r)
    case Plus(l, r)  => 1 + math.max(height(l), height(r))
    case Minus(l, r) => 1 + math.max(height(l), height(r))
    case Times(l, r) => 1 + math.max(height(l), height(r))
    case Div(l, r)   => 1 + math.max(height(l), height(r))
    case Mod(l, r)   => 1 + math.max(height(l), height(r))
    case Variable(n) => 1
  }

  // to Formatted String
  def toFormattedString(prefix: String)(e: Expr): String = e match {
    case Constant(c)             => prefix + c.toString
    case UMinus(r)               => buildUnaryExprString(prefix, "UMinus", toFormattedString(prefix + INDENT)(r))
    case Plus(l, r)              => buildExprString(prefix, "Plus", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Minus(l, r)             => buildExprString(prefix, "Minus", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Times(l, r)             => buildExprString(prefix, "Times", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Div(l, r)               => buildExprString(prefix, "Div", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Mod(l, r)               => buildExprString(prefix, "Mod", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Variable(n)             => prefix + n
    case Block(expressions @ _*) => buildExprString(prefix, "Block", expressions.map(expr => toFormattedString(prefix)(expr)): _*)
    case Cond(l, r, e)           => buildExprString(prefix, "Cond", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r), toFormattedString(prefix + INDENT)(e))
    case Loop(l, r)              => buildExprString(prefix, "Loop", toFormattedString(prefix + INDENT)(l), toFormattedString(prefix + INDENT)(r))
    case Assign(l, r)            => buildExprString(prefix, "Assign", l, toFormattedString(prefix + INDENT)(r))
  }

  def toFormattedString(e: Expr): String = toFormattedString("")(e)

  def buildExprString(prefix: String, nodeString: String, strings: String*) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    if (strings.nonEmpty) { // added protection for empty strings.head call
      result.append(strings.head)
      strings.tail.foreach { s =>
        result.append(s)
        result.append(", ")
        result.append(EOL)
      }
    }
    result.append(")")
    result.toString
  }

  def buildUnaryExprString(prefix: String, nodeString: String, exprString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(exprString)
    result.append(")")
    result.toString
  }

  // to Pretty-printer String
  def toPrettyString(prefix: String)(e: Expr): String = e match {
    case Constant(c)             => prefix + c.toString
    case UMinus(r)               => buildPUnaryExprString(prefix, "UMinus", toPrettyString(prefix)(r))
    case Plus(l, r)              => buildPExprString(false, prefix, " + ", toPrettyString(prefix)(l), toPrettyString(prefix)(r))
    case Minus(l, r)             => buildPExprString(false, prefix, " - ", toPrettyString(prefix)(l), toPrettyString(prefix)(r))
    case Times(l, r)             => buildPExprString(false, prefix, " * ", toPrettyString(prefix)(l), toPrettyString(prefix)(r))
    case Div(l, r)               => buildPExprString(false, prefix, " / ", toPrettyString(prefix)(l), toPrettyString(prefix)(r))
    case Mod(l, r)               => buildPExprString(false, prefix, " % ", toPrettyString(prefix)(l), toPrettyString(prefix)(r))
    case Variable(n)             => prefix + n
    case Block(expressions @ _*) => buildPExprString(true, prefix, "Block", expressions.map(expr => toPrettyString(prefix)(expr)): _*)
    case Cond(l, r, e)           => buildPExprString(true, prefix, "Cond", toPrettyString(prefix)(l), toPrettyString(prefix)(r), toPrettyString(prefix)(e))
    case Loop(l, r)              => buildPExprString(true, prefix, "Loop", toPrettyString(prefix)(l), toPrettyString(prefix)(r))
    case Assign(l, r)            => buildPExprString(false, prefix, " = ", l, toPrettyString(prefix)(r))
  }

  def toPrettyString(e: Expr): String = toPrettyString("")(e)

  def buildPExprString(inside: Boolean, prefix: String, nodeString: String, strings: String*) = {
    val result = new StringBuilder(prefix)
    if (inside) {
      if (nodeString == "Cond") { result.append("if(" + prefix + ") ") }
      else if (nodeString == "Loop") { result.append("while(" + prefix + ") ") }
      result.append("{\n")
      strings.tail.foreach { s =>
        result.append(s)
      }
      result.append("\n}")
    } else {
      result.append(prefix)
      if (strings.nonEmpty) {
        result.append(strings.head)
        result.append(nodeString)
        strings.tail.foreach { s =>
          result.append(s)
        }
      }
    }
    result.toString
  }

  def buildPUnaryExprString(prefix: String, nodeString: String, exprString: String) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(exprString)
    result.append(")")
    result.toString
  }

  // constants
  val EOL = scala.util.Properties.lineSeparator
  val INDENT = ".."
}
