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
  def testExecute(desc: String, ast: Expr, sB: Store) = {
    val sA = Execute.newStore
    Execute(sA)(ast)
    test("Execution -> " + desc) { assert(sA === sB) }
  }

  testExecute("assignment", simple1, store1)
  testExecute("multiple assignments", simple2, store2)
  testExecute("assignment in conditional", simple5, store5)
  testExecute("assignment in else branch", simple6, store6)
  testExecute("assignment of struct", simple11, store11)

  def testExecuteFailed(desc: String, ast: Expr, s0: Store) = {
    val res = Execute(s0)(ast)
    test("Execution failed as expected on " + desc) { assert(res.isFailure) }
  }

  testExecuteFailed("unassigned variables - 1", simple3, store0)
  testExecuteFailed("unassigned variables - 2", simple7, store0)
}
