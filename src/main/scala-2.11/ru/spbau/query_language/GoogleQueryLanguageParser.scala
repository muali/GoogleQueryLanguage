package ru.spbau.query_language

import org.parboiled2._

/**
  * Created by Maxim Moskvitin
  */

class GoogleQueryLanguageParser(val input: ParserInput) extends Parser {
  def Query = rule {ToolsQuery | CalculatorQuery | SearchQuery | Empty}

  def SearchQuery = rule {runSubParser(f => new SearchQueryParser(f).SearchQuery)}

  def CalculatorQuery = rule {runSubParser(f => new CalculatorQueryParser(f).Expression)}

  def ToolsQuery = rule {runSubParser(f => new ToolsQueryParser(f).ToolsQuery)}

  def Empty = rule {EOI}
}

object GoogleQueryLanguageParser {
  def main(args: Array[String]) {
    println(new GoogleQueryLanguageParser("\"One Two Three\" -a \"\" ~b").Query.run())
    println(new GoogleQueryLanguageParser("").Query.run())
    println(new GoogleQueryLanguageParser("asc OR bsc|asd").Query.run())
    println(new GoogleQueryLanguageParser("102..400 12.1..25.6 $102.1..$105").Query.run())

    println(new GoogleQueryLanguageParser("2+4+7-1").Query.run())
    println(new GoogleQueryLanguageParser("2*2+5").Query.run())
    println(new GoogleQueryLanguageParser("2% of 100").Query.run())
    println(new GoogleQueryLanguageParser("6 mod 4 + 4*8 mod 10 + 12% of 32").Query.run())
    println(new GoogleQueryLanguageParser("2 * 8th root of 4 ** 2 * 2").Query.run())
    println(new GoogleQueryLanguageParser("square root of 2 !").Query.run())
    println(new GoogleQueryLanguageParser("cos(5)").Query.run())
    println(new GoogleQueryLanguageParser("2 * (3 + 1)").Query.run())
    println(new GoogleQueryLanguageParser("pi * 2 kg").Query.run())
    println(new GoogleQueryLanguageParser("2 g in kg").Query.run())

    println(new GoogleQueryLanguageParser("define: word").Query.run())
    println(new GoogleQueryLanguageParser("site: google.com \"One Two Three\"").Query.run())
    println(new GoogleQueryLanguageParser("translate into into into russian").Query.run())
  }
}