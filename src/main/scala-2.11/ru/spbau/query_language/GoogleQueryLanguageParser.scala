package ru.spbau.query_language

import org.parboiled2._

/**
  * Created by Maxim Moskvitin
  */

class GoogleQueryLanguageParser(val input: ParserInput) extends Parser {
  def Query = rule {(SearchQuery | Empty) ~ EOI}

  def SearchQuery = rule {((OrQuery | AndQuery) ~ WS.*) ~> (q => queries.SearchQuery(q))}

  def AndQuery = rule {
    (SearchSubQuery ~ (!TestOr ~ WS.? ~ SearchSubQuery).*) ~>
      ((fst, other) => queries.AndQuery(fst +: other))
  }

  def OrQuery: Rule1[queries.OrQuery] = rule {
    (AndQuery ~ OrOperator ~ (OrQuery | AndQuery)) ~>
      ((lhs, rhs) => queries.OrQuery(lhs, rhs))
  }

  def SearchSubQuery: Rule1[queries.SearchSubQuery] = rule {
    QuotedPhrase | NegationWord | SynonymWord | SimpleQuery
  }

  def SimpleQuery = rule {Words ~> (s => queries.SimpleQuery(s))}

  def QuotedPhrase = rule {'\"' ~ (Words | Empty) ~ '\"' ~> (s => queries.QuotedPhrase(s))}

  def NegationWord = rule {'-' ~ capture(Word) ~> (s => queries.NegationWord(s))}

  def SynonymWord = rule {'~' ~ capture(Word) ~> (s => queries.SynonymWord(s))}

  def OrOperator = rule {(WS.+ ~ "OR" ~ WS.+) | (WS.* ~ '|' ~ WS.*)}

  def TestOr = rule {OrOperator ~ SearchSubQuery}

  def Empty = rule {push("")}

  def Words = rule {capture(Word ~ (!TestOr ~ WS.? ~ Word).*)}

  def WS = rule {quiet(anyOf(" \t \n").+)}

  def Word = rule {CharPredicate.AlphaNum.+}
}

object GoogleQueryLanguageParser {
  def main(args: Array[String]) {
    println(new GoogleQueryLanguageParser("\"One Two Three\" -a \"\" ~b").Query.run())
    println(new GoogleQueryLanguageParser("").Query.run())
    println(new GoogleQueryLanguageParser("asc OR bsc|asd").Query.run())
  }
}