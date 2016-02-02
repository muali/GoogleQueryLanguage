package ru.spbau.query_language

import org.parboiled2._

/**
  * Created by Maxim Moskvitin
  */
abstract class BasicParser extends Parser {
  def WS = rule {quiet(anyOf(" \t \n").+)}
  def Word = rule {CharPredicate.AlphaNum.+}
}
