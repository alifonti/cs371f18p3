package edu.luc.cs.laufer.cs473.expressions

import org.scalatest.FunSuite
import TestFixtures._
import edu.luc.cs.laufer.cs473.expressions.ast.Expr

object MainCombinatorParser extends App {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  println(parsedExpr.get)
  println(complex1)
  println(parsedExpr.get == complex1)
  println(behaviors.evaluate(parsedExpr.get))
}

class TestCombinatorParser extends FunSuite {
  def testParser(s: String, ast: Expr) = {
    val parsedExpr = CombinatorParser.parseAll(CombinatorParser.topLevel, s)
    test("Parser works on " + s) { assert(parsedExpr.get === ast) }
  }

  testParser(simple1string, simple1)
  testParser(simple2string, simple2)
  testParser(simple3string, simple3)
  testParser(simple4string, simple4)
  testParser(simple5string, simple5)
  testParser(simple6string, simple6)
  testParser(simple7string, simple7)
  testParser(simple8string, simple8)
  testParser(simple9string, simple9)
  testParser(simple10string, simple10)
  testParser(simple11string, simple11)
}
