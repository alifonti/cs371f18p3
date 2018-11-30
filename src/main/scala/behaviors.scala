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
    case Assign(l, r)            => buildExprString(prefix, "Assign", l.head, toFormattedString(prefix + INDENT)(r))
    case Select(f @ _*)          => buildSelectString(prefix, "Select", f)
    case Struct(fields @ _*)     => buildStructString(prefix, "Struct", fields)
  }

  def toFormattedString(e: Expr): String = toFormattedString("")(e)

  def buildExprString(prefix: String, nodeString: String, strings: String*) = {
    val result = new StringBuilder(prefix)
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    if (strings.nonEmpty) {
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

  def buildSelectString(prefix: String, nodeString: String, strings: Seq[String]) = {
    val result = new StringBuilder()
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    result.append(strings.mkString("."))
    result.append(")")
    result.toString()
  }

  def buildStructString(prefix: String, nodeString: String, fields: Seq[(String, Expr)]) = {
    val result = new StringBuilder()
    result.append(nodeString)
    result.append("(")
    result.append(EOL)
    fields.foreach(f => {
      result.append(f._1)
      result.append(": ")
      result.append(toFormattedString(f._2))
      result.append(",")
    })
    result.append(")")
    result.toString()
  }

  // to Pretty-printer String
  def toPrettyString(prefix: String)(e: Expr): String = e match {
    case Constant(c)             => c.toString //prefix + c.toString
    case UMinus(r)               => buildPUnaryExprString(prefix, "-", toPrettyString(prefix)(r))
    case Plus(l, r)              => buildPExprString(prefix, " + ", toPrettyString(prefix)(l), toPrettyString(prefix)(r))
    case Minus(l, r)             => buildPExprString(prefix, " - ", toPrettyString(prefix)(l), toPrettyString(prefix)(r))
    case Times(l, r)             => buildPExprString(prefix, " * ", toPrettyString(prefix)(l), toPrettyString(prefix)(r))
    case Div(l, r)               => buildPExprString(prefix, " / ", toPrettyString(prefix)(l), toPrettyString(prefix)(r))
    case Mod(l, r)               => buildPExprString(prefix, " % ", toPrettyString(prefix)(l), toPrettyString(prefix)(r))
    case Variable(n)             => n
    case Block(expressions @ _*) => buildPBlockString(prefix, "Block", expressions) //.map(expr => toPrettyString(prefix)(expr)): _*)
    case Cond(l, r, e)           => buildPCondString(prefix, "if(", toPrettyString(prefix)(l), toPrettyString(prefix)(r), toPrettyString(prefix)(e))
    case Loop(l, r)              => buildPLoopString(prefix, "while(", toPrettyString(prefix)(l), toPrettyString(prefix)(r))
    case Assign(l, r)            => buildPAssignString(prefix, " = ", l, toPrettyString(prefix)(r))
    case Select(f @ _*)          => buildPSelectString(prefix, f)
    case Struct(fields @ _*)     => buildPStructString(prefix, fields)
  }

  def toPrettyString(e: Expr): String = toPrettyString("")(e)

  // builders
  def buildPExprString(prefix: String, nodeString: String, l: String, r: String) = {
    val result = new StringBuilder()
    result.append("(")
    result.append(l)
    result.append(nodeString)
    result.append(r)
    result.append(")")
    result.toString
  }

  def buildPAssignString(prefix: String, nodeString: String, l: Seq[String], r: String) = {
    val result = new StringBuilder()
    result.append(l.mkString("."))
    result.append(nodeString)
    result.append(r)
    result.append(";")
    result.append(EOL)
    result.toString
  }

  def buildPBlockString(prefix: String, nodeString: String, exprs: Seq[Expr]) = {
    val result = new StringBuilder()
    result.append("{\n")
    exprs.foreach { e =>
      result.append(prefix + "  ")
      result.append(toPrettyString(prefix + "  ")(e))
    }
    result.append(prefix)
    result.append("}")
    result.append(EOL)
    result.toString
  }

  def buildPStructString(prefix: String, fields: Seq[(String, Expr)]) = {
    val result = new StringBuilder()
    val EvaluatedPairs: Seq[(String, String)] = fields.map { case (s, e) => (s, toPrettyString(prefix)(e)) }
    if (EvaluatedPairs.nonEmpty) {
      EvaluatedPairs.foreach(p => {
        result.append("{")
        result.append(p._1)
        result.append(": ")
        result.append(p._2)
        result.append("}")
      })
    } else {
      result.append("{ }")
    }
    result.toString()
  }

  def buildPSelectString(prefix: String, fields: Seq[String]) = {
    val result = new StringBuilder()
    result.append(fields.mkString("."))
    result.toString()
  }

  def buildPLoopString(prefix: String, nodeString: String, l: String, r: String) = {
    val result = new StringBuilder()
    result.append(nodeString)
    result.append(l)
    result.append(")")
    result.append(r)
    result.toString
  }

  def buildPCondString(prefix: String, nodeString: String, l: String, r: String, e: String) = {
    val result = new StringBuilder()
    result.append(nodeString)
    result.append(l)
    result.append(")")
    result.append(r)
    result.append(prefix)
    result.append("else")
    result.append(e)
    result.toString
  }

  def buildPUnaryExprString(prefix: String, nodeString: String, exprString: String) = {
    val result = new StringBuilder()
    result.append(nodeString)
    result.append(exprString)
    result.toString
  }

  // constants
  val EOL = scala.util.Properties.lineSeparator
  val INDENT = ".."
}
