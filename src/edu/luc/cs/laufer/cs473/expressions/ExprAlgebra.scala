package edu.luc.cs.laufer.cs473.expressions

/**
 * The category of expression algebras.
 */
trait ExprAlgebra[R] {
  def visitConstant(c: Constant): R
  def visitPlus(l: R, r: R, e: Plus): R
  def visitMinus(l: R, r: R, e: Minus): R
  def visitTimes(l: R, r: R, e: Times): R
  def visitDiv(l: R, r: R, e: Div): R
}

/**
 * The catamorphism for expressions.
 */
class ExprFold[R] {
  def apply(v: ExprAlgebra[R])(e: Expr): R = e match {
    case c: Constant => v.visitConstant(c)
    case e: Plus => v.visitPlus(this(v)(e.left), this(v)(e.right), e)
    case e: Minus => v.visitMinus(this(v)(e.left), this(v)(e.right), e)
    case e: Times => v.visitTimes(this(v)(e.left), this(v)(e.right), e)
    case e: Div => v.visitDiv(this(v)(e.left), this(v)(e.right), e)
  }
}