package edu.luc.cs.laufer.cs473.expressions

import org.scalatest.FunSuite

import TestFixtures._

object MainCombinatorParser extends App {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  println(parsedExpr.get)
  println(complex1)
  println(parsedExpr.get == complex1)
  println(behaviors.evaluate(parsedExpr.get))
}

class TestCombinatorParser extends FunSuite {
  val parsedExpr = CombinatorParser.parseAll(CombinatorParser.expr, complex1string)
  val parsedExpr2 = CombinatorParser.parseAll(CombinatorParser.expr, complex1string2)
  val parsedExpr3 = CombinatorParser.parseAll(CombinatorParser.statement, simpleAssignmentString)
  val parsedExpr4 = CombinatorParser.parseAll(CombinatorParser.expr, simpleVariableString)
  //val parsedExpr5 = CombinatorParser.parseAll(CombinatorParser.statement, complexExpressionString)
  test("parser works 1") { assert(parsedExpr.get === complex1) }
  test("parser works 2") { assert(parsedExpr2.get === complex1) }
  test("parser works 3") { assert(parsedExpr3.get === simpleAssignment)}
  test("parser works 4") { assert(parsedExpr4.get === simpleVariable)}
  //test("parser works 5") { assert(parsedExpr5.get === complexExpression)}
}
