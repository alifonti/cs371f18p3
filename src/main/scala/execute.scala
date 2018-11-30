package edu.luc.cs.laufer.cs473.expressions

import ast._


import scala.collection.mutable.{Map => MMap}
import scala.util.{Failure, Success, Try}

/** A run-time value is always a number for now. We represent NULL as 0. */
sealed trait Value
case class Num(value: Int) extends Value
case class Ins(value: MMap[String, Value]) extends Value

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
      store <- Try(left.init.foldLeft(store)((r, f) => r(f).asInstanceOf[Ins].value)) //this gives the unwrapped value from an ins, which is an MMap
    } yield {
      left.foreach(store.put(_, rvalue)) //uncertain about this, I just removed the error for now
      Value.NULL
    }
    //Allan helped me understand Cond, Block, and Loop concepts (specifically, how to use the Success "wrapper" in case matching). Thanks Allan!
    case Cond(guard, thenBranch, elseBranch) => {
      apply(store)(guard) match {
        case Success(Value.NULL)    => apply(store)(elseBranch)
        case Success(_)             => apply(store)(thenBranch)
        case f @ Failure(exception) => f
      }
    }
    case Block(statements @ _*) => {
      val it = statements.iterator
      var unwrapped: Value = Value.NULL //notice this mutable variable will be assigned to the unwrapped value on line 70
      while (it.hasNext) {
        apply(store)(it.next()) match {
          case Success(r)             => unwrapped = r
          case f @ Failure(exception) => return f
        }
      }
      Success(unwrapped) //re-wrap the value to be returned
    }
    case Loop(guard, body) => {
      var gValue: Value = Value.NULL
      while (true) { //this is basically like "continually"
        apply(store)(guard) match { //case matching on the guard of the loop
          case Success(Value.NULL) => return Success(Value.NULL)
          case Success(g) => apply(store)(body) match {
            case Success(b)             => Success(b) //case matching on the body of the loop
            case f @ Failure(exception) => return f
          }
          case f @ Failure(exception) => return f
        }
      }
      Success(Value.NULL) //loops always evaluated as NULL
    }

    //also worked on this during Dr. L office hours 11/29
    case Select(items @ _*) => Try {
      val s = items.init.foldLeft(store)((r, f) =>
        r(f).asInstanceOf[Ins].value) //this gives the unwrapped value from an ins, which is an MMap
      s(items.last)
    }

    case Struct(fields @ _*) => {
      // have: Seq[(String, Result)] = Seq[(String, Success[Value])] as long as everything succeeds
      // want: Result where we have Success(Ins(Map[String, Value]))
      Try(Ins(MMap(fields.map { case (k, v) => (k, Execute(store)(v).get) }: _*)))
    }
  }
}
