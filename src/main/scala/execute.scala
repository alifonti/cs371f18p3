package edu.luc.cs.laufer.cs473.expressions

import ast._

import scala.collection.mutable.{Map => MMap}
import scala.util.{Success, Try}

/** A run-time value is always a number for now. We represent NULL as 0. */
sealed trait Value
case class Num(value: Int) extends Value

/** A companion object defining a useful Value instance. */
object Value {
  val NULL = Num(0)
}

/** An interpreter for expressions and statements. */
object Execute {

  type Store = MMap[String, Value]
  type Result = Try[Value]

  def newStore: Store = MMap.empty[String, Value]

  def apply(store: Store = MMap.empty[String, Value])(s: Expr): Result = s match {
    case Constant(value) => Success(Num(value))
    case Plus(left, right) => for {
      l <- apply(store)(left)
      r <- apply(store)(right)
    } yield Num(l.asInstanceOf[Num].value + r.asInstanceOf[Num].value)
    case UMinus(left) => for {
      l <- apply(store)(left)
    } yield Num(-l.asInstanceOf[Num].value)
    case Minus(left, right) => for {
      l <- apply(store)(left)
      r <- apply(store)(right)
    } yield Num(l.asInstanceOf[Num].value - r.asInstanceOf[Num].value)
    case Times(left, right) => for {
      l <- apply(store)(left)
      r <- apply(store)(right)
    } yield Num(l.asInstanceOf[Num].value * r.asInstanceOf[Num].value)
    case Div(left, right) => for {
      l <- apply(store)(left)
      r <- apply(store)(right)
    } yield Num(l.asInstanceOf[Num].value / r.asInstanceOf[Num].value)
    case Mod(left, right) => for {
      l <- apply(store)(left)
      r <- apply(store)(right)
    } yield Num(l.asInstanceOf[Num].value % r.asInstanceOf[Num].value)
    case Variable(name) => Try(store(name))
    case Assign(left, right) => for {
      rvalue <- apply(store)(right)
    } yield {
      store.put(left, rvalue)
      Value.NULL
    }
    //below not completely working yet
    case Block(statements @ _*) =>
      statements.foldLeft[Result](Success(Value.NULL))((_, s) => apply(store)(s))

    //below does not work yet
//    case Loop(guard, body) =>
//      var gValue = apply(store)(guard)
//      while (gValue.asInstanceOf[Num] != Value.NULL) {
//        apply(store)(body)
//        gValue = apply(store)(guard)
//      }
//      Value.NULL
  }
}
