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
    );

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
    );

  val simpleAssignmentString = "x = y;"

  val simpleAssignment = Assignment(
    Variable("x"),
    Variable("y")
    //Constant(5)
  );

  val complexAssignmentString = "x = 5 ; y = 7;"

  val complexExpressionString = "((1 + y2) - (3 * y4)) / 5;"

  val complexExpression =
    Div(
      Minus(
        Plus(
          Constant(1),
          Variable("y2")
        ),
        Plus(
          Constant(3),
          Variable("y4")
        )
      ),
      Constant(5)
    )

  val complexAssignment2String = "x = ((1 + y2) - (3 * y4)) / 5;"

  val simpleVariableString = "x"

  val simpleVariable = Variable("x")

}
