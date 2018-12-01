package edu.luc.cs.laufer.cs473.expressions

import edu.luc.cs.laufer.cs473.expressions.Execute.Store
import scala.collection.mutable.{Map => MMap}

object TestFixtures {

  import ast._

  val complex1 =
    Div(
      Minus(
        Plus(
          Constant(1),
          Constant(2)
        ),
        Times(
          Constant(3),
          Constant(4)
        )
      ),
      Constant(5)
    )

  val complex1string = "((1 + 2) - (3 * 4)) / 5"

  val complex1string2 = "  ((1 + 2) - (3 * 4)) / 5  "

  val complex2 =
    Mod(
      Minus(
        Plus(
          Constant(1),
          Constant(2)
        ),
        Times(
          UMinus(
            Constant(3)
          ),
          Constant(4)
        )
      ),
      Constant(5)
    )

  val store0: Store = Execute.newStore //this store is empty

  // simple1 Test
  val store1: Store = Execute.newStore //this store is also empty, but won't be after the following line
  store1.put("x", Num(5))

  val simple1 = Block(
    Assign(
      Seq[String]("x"), Constant(5)
    )
  )

  val simple1string = "x = 5;"

  // simple2 Test
  val store2: Store = Execute.newStore
  store2.put("x", Num(4))
  store2.put("y", Num(6))

  val simple2 = Block(
    Assign(
      Seq[String]("x"), Constant(4),
    ),
    Assign(
      Seq[String]("y"), Constant(6),
    ),
  )

  val simple2string = "x = 4 ; y = 6;"

  // simple3 Test
  val simple3 = Block(
    Div(
      Minus(
        Plus(
          Constant(1), Variable("y2"),
        ), Times(
          Constant(3), Variable("y4"),
        ),
      ), Constant(5),
    ))


  val simple3string = "((1 + y2) - (3 * y4)) / 5;"

  // simple4 Test
  val simple4 = Block(
    Assign(
      Seq[String]("x"), Div(
        Minus(
          Plus(
            Constant(1), Variable("y2"),
          ), Times(
            Constant(3), Variable("y4"),
          ),
        ), Constant(5),
      ),
    ))

  val simple4string = "x = ((1 + y2) - (3 * y4)) / 5;"

  // simple5 Test
  val store5: Store = Execute.newStore
  store5.put("x", Num(2))

  val simple5 = Block(
    Cond(
      Constant(1), Block(
        Assign(
          Seq[String]("x"), Constant(2),
        )),
      Block(
      ),
    ))


  val simple5string = "if (1) { x = 2; }"

  // simple6 Test
  val store6: Store = Execute.newStore
  store6.put("x", Num(2))

  val simple6 = Block(
    Cond(
      Constant(1), Block(
        Assign(
          Seq[String]("x"), Constant(2),
        )),
      Block(
        Assign(
          Seq[String]("x"), Constant(3),
        )),
    ))

  val simple6string = "if (1) { x = 2; } else { x = 3; }"

  // simple7 Test
  val simple7 = Block(
    Block(
      Assign(
        Seq[String]("r"), Plus(
          Variable("r"), Variable("x"),
        ),
      ), Assign(
        Seq[String]("y"), Plus(
          Variable("y"), Constant(1),
        ),
      ),
    ))


  val simple7string = "{ r = r + x; y = y + 1 ; }"

  // simple8 Test
  val simple8 = Block(
    Cond(
      Constant(4), Block(
        Assign(
          Seq[String]("r"), Plus(
            Variable("r"), Variable("x"),
          ),
        ), Assign(
          Seq[String]("y"), Plus(
            Variable("y"), Constant(1),
          ),
        ),
      ),
      Block(
      ),
    ))

  val simple8string = "if (4) { r = r + x; y = y + 1; }"

  //simple9 and simple10 are identical AST's. simple9string and simple10string are identical but for whitespace characters
  // simple9 and simple10 Tests
  val simple9 = Block(
    Loop(
      Variable("y"), Block(
        Assign(
          Seq[String]("r"), Plus(
            Variable("r"), Variable("x"),
          ),
        ), Assign(
          Seq[String]("y"), Minus(
            Variable("y"), Constant(1),
          ),
        ),
      ),
    ))

  val simple9string = "while (y) { r = r + x; y = y - 1; }"

  val simple10 = Block(
    Loop(
      Variable("y"), Block(
        Assign(
          Seq[String]("r"), Plus(
            Variable("r"), Variable("x"),
          ),
        ), Assign(
          Seq[String]("y"), Minus(
            Variable("y"), Constant(1),
          ),
        ),
      ),
    ))

  val simple10string = "while (y) { r = r + x ; y = y - 1 ;}"

  val store11: Store = Execute.newStore
  store11.put("x", Ins(MMap.empty))

  val simple11 = Block(
    Assign(
      Seq[String]("x"), Struct(),
    )
  )

  val simple11string = "x = {};"
}
