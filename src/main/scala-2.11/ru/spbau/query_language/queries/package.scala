package ru.spbau.query_language

/**
  * Created by Maxim Moskvitin
  */
package object queries {

  trait SearchSubQuery {
  }

  trait HighLvlSearchQuery {
  }

  case class QuotedPhrase(phrase: String) extends SearchSubQuery {
  }

  case class NegationWord(word: String) extends SearchSubQuery {
  }

  case class SynonymWord(word: String) extends SearchSubQuery {
  }

  case class SimpleQuery(query: String) extends SearchSubQuery {
  }

  case class SearchQuery(query: HighLvlSearchQuery) {
  }

  case class AndQuery(subQueries: Seq[SearchSubQuery]) extends HighLvlSearchQuery {
  }

  case class OrQuery(lhs: HighLvlSearchQuery, rhs: HighLvlSearchQuery) extends HighLvlSearchQuery {
  }

}
