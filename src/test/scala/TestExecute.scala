package edu.luc.cs.laufer.cs473.expressions

import org.scalatest.FunSuite
import TestFixtures._
import edu.luc.cs.laufer.cs473.expressions.Execute.Store
import edu.luc.cs.laufer.cs473.expressions.ast.Expr

object MainExecute extends App {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  println(parsedExpr.get)
  println(complex1)
  println(parsedExpr.get == complex1)
  println(behaviors.evaluate(parsedExpr.get))
}

class TestExecute extends FunSuite {
  def testExecute(desc: String, ast: Expr, s0: Store, s1: Store) = {
    Execute(s0)(ast)
    test("Execution works on " + desc) { assert(s0 === s1) }
  }

  testExecute("assignment", simple1, store0, store1)
  //testExecute("assignment", simple1, store0, store1)
}
