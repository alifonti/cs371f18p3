package edu.luc.cs.laufer.cs473.expressions

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

  //new test cases start here

  val simple1 = Block(
    Assign(
      "x", Constant(5)
    )
  )

  val simple1string = "x = 5;"

  val simple2 = Block(
    Assign(
      "x", Constant(5),
    ),
    Assign(
      "y", Constant(7),
    ),
  )

  val simple2string = "x = 5 ; y = 7;"

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

  val simple4 = Block(
    Assign(
      "x", Div(
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

  val simple5 = Block(
    Cond(
      Constant(1), Block(
        Assign(
          "x", Constant(2),
        )),
      Block(
      ),
    ))


  val simple5string = "if (1) { x = 2; }"

  val simple6 = Block(
    Cond(
      Constant(1), Block(
        Assign(
          "x", Constant(2),
        )),
      Block(
        Assign(
          "x", Constant(3),
        )),
    ))

  val simple6string = "if (1) { x = 2; } else { x = 3; }"

  val simple7 = Block(
    Block(
      Assign(
        "r", Plus(
          Variable("r"), Variable("x"),
        ),
      ), Assign(
        "y", Plus(
          Variable("y"), Constant(1),
        ),
      ),
    ))


  val simple7string = "{ r = r + x; y = y + 1 ; }"

  val simple8 = Block(
    Cond(
      Constant(4), Block(
        Assign(
          "r", Plus(
            Variable("r"), Variable("x"),
          ),
        ), Assign(
          "y", Plus(
            Variable("y"), Constant(1),
          ),
        ),
      ),
      Block(
      ),
    ))

  val simple8string = "if (4) { r = r + x; y = y + 1; }"

  //simple9 and simple10 are identical AST's. simple9string and simple10string are identical but for whitespace characters

  val simple9 = Block(
    Loop(
      Variable("y"), Block(
        Assign(
          "r", Plus(
            Variable("r"), Variable("x"),
          ),
        ), Assign(
          "y", Minus(
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
          "r", Plus(
            Variable("r"), Variable("x"),
          ),
        ), Assign(
          "y", Minus(
            Variable("y"), Constant(1),
          ),
        ),
      ),
    ))

  val simple10string = "while (y) { r = r + x ; y = y - 1 ;}"
}
