package ru.spbau.query_language

import org.parboiled2._

import scala.annotation.tailrec

/**
  * Created by Maxim Moskvitin
  */
object Expressions {
  trait Expression {
  }

  case class Addition(lhs: Expression, rhs: Expression) extends Expression {
  }

  case class Substraction(lhs: Expression, rhs: Expression) extends Expression {
  }

  case class Multiplication(lhs: Expression, rhs: Expression) extends Expression {
  }

  case class Division(lhs: Expression, rhs: Expression) extends Expression {
  }

  case class Percent(lhs: Expression, rhs: Expression) extends Expression {
  }

  case class Modulo(lhs: Expression, rhs: Expression) extends Expression {
  }

  case class Exponentiation(arg: Expression, power: Expression) extends Expression {
  }

  case class Root(radicant: Expression, index: Expression) extends Expression {
  }

  case class Choose(n: Expression, k: Expression) extends Expression {
  }

  case class Factorial(operand: Expression) extends Expression {
  }

  case class FunctionCall(name: String, arg: Expression) extends Expression {
  }

  case class Constant(name: String) extends Expression {
  }

  case class Decimal(value: String) extends Expression {
  }

  case class Measure(value: Decimal, units: String) extends Expression {
  }

  case class Conversion(e: Expression, units: String) extends Expression {
  }

}

object InfixHelper {
  case class OperatorStorage(op: String, rhs: Expressions.Expression) {
  }

  def makeInfix(lhs: Expressions.Expression, rhs: Expressions.Expression, op: String) = op match {
    case "+" => Expressions.Addition(lhs, rhs)
    case "-" => Expressions.Substraction(lhs, rhs)
    case "*" => Expressions.Multiplication(lhs, rhs)
    case "/" => Expressions.Division(lhs, rhs)
    case "mod" => Expressions.Modulo(lhs, rhs)
    case "% of" => Expressions.Percent(lhs, rhs)
    case "^" | "**" => Expressions.Exponentiation(lhs, rhs)
    case "th root of" => Expressions.Root(rhs, lhs)
    case "choose" => Expressions.Choose(rhs, lhs)
  }

  //TODO: replace with foldl
  @tailrec
  def foldExpression(lhs: Expressions.Expression, rhss: Seq[OperatorStorage]): Expressions.Expression = rhss match {
    case Nil => lhs
    case Seq(x) => makeInfix(lhs, x.rhs, x.op)
    case Seq(x, xs@_*) => foldExpression(makeInfix(lhs, x.rhs, x.op), xs)
  }

}

class CalculatorQueryParser(val input: ParserInput) extends Parser {
  def Expression = rule {WS.* ~ (Conversion | Infix1) ~ EOI}

  def Conversion = rule {Infix1 ~ "in" ~ WS.* ~ Units ~> ((e, u) => Expressions.Conversion(e, u))}

  def Infix(term: () => Rule1[Expressions.Expression], sign: () => Rule1[String]) = rule {
    term() ~ (sign() ~ term() ~> ((op, e) => InfixHelper.OperatorStorage(op, e))).* ~>
      ((lhs, rhss) => InfixHelper.foldExpression(lhs, rhss))
  }

  def Infix1: Rule1[Expressions.Expression] = rule { Infix(() => Infix2, () => Sign1) }

  def Sign1 = rule {capture("+" | "-") ~ WS.*}

  def Infix2 = rule { Infix(() => Infix3, () => Sign2) }

  def Sign2 = rule {capture("*" | "/" | "mod" | "% of" | "%") ~ WS.*}

  def Infix3 = rule { Infix(() => Postfix, () => Sign3)}

  def Sign3 = rule {capture("th root of" | "choose" | "^" | "**") ~ WS.*}

  def Postfix = rule {
    (Prefix ~ "!" ~ WS.* ~> (e => Expressions.Factorial(e))) |
      Prefix
  }

  def Prefix = rule {
    (Root ~ BasicTerm ~> ((i, r) => Expressions.Root(r, i))) |
      BasicTerm
  }

  def Root = rule { SquareRoot | CubeRoot }

  def SquareRoot = rule {"square root of" ~ WS.* ~ push(Expressions.Decimal("2"))}

  def CubeRoot = rule {"cube root of" ~ WS.* ~ push(Expressions.Decimal(""))}

  def BasicTerm = rule {FunctionCall | Parens | Constant | Measure | Number}

  def FunctionCall = rule {
    capture(CharPredicate.Alpha.+) ~ WS.* ~ "(" ~ WS.* ~ Infix1 ~ ")" ~ WS.* ~>
      ((name, e) => Expressions.FunctionCall(name, e))
  }

  def Parens = rule {"(" ~ WS.* ~ Infix1 ~ ")" ~ WS.*}

  //Physical constants is skipped - too much
  def Constant = rule {capture("e" | "pi" | "i" | "gamma") ~ WS.* ~> (s => Expressions.Constant(s))}

  def Number = rule {Decimal}

  def Measure = rule {Number ~ Units ~> ((d, u) => Expressions.Measure(d, u))}

  def Units = rule {capture("g" | "kg" | "tons") ~ WS.*}

  def Decimal = rule {
    capture (CharPredicate.Digit.+ ~ ('.' ~ CharPredicate.Digit.*).?) ~ WS.* ~> (s => Expressions.Decimal(s))
  }

  def WS = rule {quiet(anyOf(" \t \n").+)}
}
