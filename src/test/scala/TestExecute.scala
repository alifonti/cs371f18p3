package edu.luc.cs.laufer.cs473.expressions

import org.scalatest.FunSuite
import TestFixtures._
import edu.luc.cs.laufer.cs473.expressions.ast.Expr

object MainExecute extends App {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  println(parsedExpr.get)
  println(complex1)
  println(parsedExpr.get == complex1)
  println(behaviors.evaluate(parsedExpr.get))
}

class TestExecute extends FunSuite {
  def testExecute(s: String, ast: Expr) = { //this test not yet fully implemented
    val parsedExpr = CombinatorParser.parseAll(CombinatorParser.topLevel, s)
    test("Executor/Interpreter works on " + s) { assert(parsedExpr.get === ast) }
  }

//  testExecute(simple1string, simple1)
//  testExecute(simple2string, simple2)
//  testExecute(simple3string, simple3)
//  testExecute(simple4string, simple4)
//  testExecute(simple5string, simple5)
//  testExecute(simple6string, simple6)
//  testExecute(simple7string, simple7)
//  testExecute(simple8string, simple8)
//  testExecute(simple9string, simple9)
//  testExecute(simple10string, simple10)
}
