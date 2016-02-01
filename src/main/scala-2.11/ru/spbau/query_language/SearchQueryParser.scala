package ru.spbau.query_language

import org.parboiled2._

/**
  * Created by Maxim Moskvitin
  */

object queries {

  trait SearchSubQuery {
  }

  trait HighLvlSearchQuery {
  }

  trait SubPhrase {
  }

  case class Word(word: String) extends SearchSubQuery with SubPhrase {
  }

  case class Placeholder() extends SubPhrase {
  }

  case class QuotedPhrase(subPhrases: Seq[SubPhrase]) extends SearchSubQuery {
  }

  case class NegationWord(word: String) extends SearchSubQuery {
  }

  case class SynonymWord(word: String) extends SearchSubQuery {
  }

  case class RangeQuery(from: Double, to: Double, literal: Option[String]) extends SearchSubQuery {
  }

  case class SimpleQuery(query: String) extends SearchSubQuery {
  }

  case class SearchQuery(query: HighLvlSearchQuery) {
  }

  case class AndQuery(subQueries: Seq[SearchSubQuery]) extends HighLvlSearchQuery {
  }

  case class OrQuery(subQueries: Seq[AndQuery]) extends HighLvlSearchQuery {
  }

}

class SearchQueryParser(val input: ParserInput) extends Parser {

  def SearchQuery = rule {((OrQuery | AndQuery) ~ WS.*) ~ EOI ~> (q => queries.SearchQuery(q))}

  def AndQuery = rule {
    (SearchSubQuery ~ (!TestOr ~ WS.? ~ SearchSubQuery).*) ~>
      ((fst, other) => queries.AndQuery(fst +: other))
  }

  def OrQuery = rule {
    (AndQuery ~ (OrOperator ~ AndQuery).+) ~> ((fst, other) => queries.OrQuery(fst +: other))
  }

  def SearchSubQuery: Rule1[queries.SearchSubQuery] = rule {
    QuotedPhrase | NegationWord | SynonymWord | RangeQuery | WordQuery
  }

  def QuotedPhrase = rule {
    WS.* ~ '\"' ~ SubPhrase.* ~ '\"' ~> (seq => queries.QuotedPhrase(seq))
  }

  def SubPhrase = rule {Placeholder | WordQuery}

  def WordQuery = rule {WS.* ~ capture(Word) ~> (s => queries.Word(s))}

  def NegationWord = rule {WS.* ~ '-' ~ capture(Word) ~> (s => queries.NegationWord(s))}

  def SynonymWord = rule {WS.* ~ '~' ~ capture(Word) ~> (s => queries.SynonymWord(s))}

  def RangeQuery = rule {
    WS.* ~ (SimpleRangeQuery | LiteralRangeQuery)
  }

  def SimpleRangeQuery = rule {
    capture(Float) ~ ".." ~ capture(Float) ~> ((from, to) => queries.RangeQuery(from.toDouble, to.toDouble, None))
  }

  //Looks like only $ is acceptable
  def LiteralRangeQuery = rule {
    '$' ~ capture(Float) ~ ".." ~ '$' ~ capture(Float) ~>
      ((from, to) => queries.RangeQuery(from.toDouble, to.toDouble, Some("$")))
  }

  def OrOperator = rule {(WS.+ ~ "OR" ~ WS.+) | (WS.* ~ '|' ~ WS.*)}

  def TestOr = rule {OrOperator ~ SearchSubQuery}

  def WS = rule {quiet(anyOf(" \t \n").+)}

  def Word = rule {CharPredicate.AlphaNum.+}

  def Float = rule {CharPredicate.Digit.+ ~ ('.' ~ CharPredicate.Digit.+).?}

  def Placeholder = rule {WS.* ~ '*' ~> queries.Placeholder}
}
