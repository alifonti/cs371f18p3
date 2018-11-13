package edu.luc.cs.laufer.cs473.expressions

import scala.util.control.Breaks._

import org.jline.reader.LineReaderBuilder
import org.jline.terminal.TerminalBuilder

object CombinatorCalculator extends App {

  val terminal = TerminalBuilder.terminal
  val reader = LineReaderBuilder.builder.terminal(terminal).build
  val prompt = "Parser> "

  val store = Execute.newStore

  def processExpr(input: String): Unit = {
    println("You entered: " + input)
    val result = CombinatorParser.parseAll(CombinatorParser.topLevel, input)
    //val result = CombinatorParser.parseAll(CombinatorParser.expr, input)
    if (result.isEmpty) {
      println("This expression could not be parsed")
    } else {
      import behaviors._
      val expr = result.get
      println("The parsed expression is: ")
      println(toFormattedString(expr))
      println("The un-parsed statements are: ")
      println(toPrettyString(expr))

      //println("It has size " + size(expr) + " and height " + height(expr))
      println("Memory: " + store)
      println("It evaluates to " + Execute(store)(expr))
      println("Memory: " + store)
    }
  }

  if (args.length > 0) {
    processExpr(reader.readLine(prompt))
  } else {
    breakable {
      while (true) {
        try { processExpr(reader.readLine(prompt)) }
        catch { case _: Throwable => break } }
    }
  }
}
