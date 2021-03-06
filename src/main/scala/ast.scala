package edu.luc.cs.laufer.cs473.expressions.ast

/** An initial algebra of arithmetic expressions. */
sealed trait Expr
case class Constant(value: Int) extends Expr
case class UMinus(expr: Expr) extends Expr
case class Plus(left: Expr, right: Expr) extends Expr
case class Minus(left: Expr, right: Expr) extends Expr
case class Times(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr
case class Mod(left: Expr, right: Expr) extends Expr

case class Variable(variable:String) extends Expr
case class Assignment(left:Variable, right:Expr) extends Expr
case class Conditional(guard:Expr, ifBranch:Expr, elseBranch:Option[Expr] = None) extends Expr
case class Loop(guard:Expr, body:Expr) extends Expr
case class Block(expressions:Expr*) extends Expr
