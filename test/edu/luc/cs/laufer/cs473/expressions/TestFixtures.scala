package edu.luc.cs.laufer.cs473.expressions

object TestFixtures {

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
}