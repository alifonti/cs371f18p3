package edu.luc.cs.laufer.cs473.expressions

import scala.util.control.Breaks._

import org.jline.reader.LineReaderBuilder
import org.jline.terminal.TerminalBuilder

object CombinatorCalculator extends App {

  val terminal = TerminalBuilder.terminal
  val reader = LineReaderBuilder.builder.terminal(terminal).build
  val prompt = "Enter infix expression: "

  val store = Execute.newStore

  def processExpr(input: String): Unit = {
    println("You entered: " + input)
    val result = CombinatorParser.parseAll(CombinatorParser.topLevel, input)
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
      println("It evaluates to " + Execute(store)(expr))
      println("Memory: " + store + "\n") //Sakai example prints updated map before and after each expression entered. I added a newline character to make this clearer
    }
  }

  if (args.length > 0) {
    processExpr(args mkString " ")
  } else {
    breakable {
      while (true) {
        println("Memory: " + store) //print the contents of the store immediately prior to prompting the user for an expression
        try { processExpr(reader.readLine(prompt)) }
        catch { case _: Throwable => break }
      }
    }
  }
}
