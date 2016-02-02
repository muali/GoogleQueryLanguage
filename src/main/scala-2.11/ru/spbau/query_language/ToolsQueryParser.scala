package ru.spbau.query_language

import org.parboiled2._

/**
  * Created by Maxim Moskvitin
  */
object Tools {
  case class Definition(w: String) {
  }

  case class SiteSearch(link: String, query: queries.SearchQuery) {
  }

  case class TranslationQuery(phrase: String, language: String) {
  }
}

class ToolsQueryParser(val input: ParserInput) extends BasicParser  {
  def ToolsQuery = rule {WS.* ~ (Translation | Definition | SiteSearch) ~ EOI}

  def SearchQuery = rule {runSubParser(f => new SearchQueryParser(f).SearchQuery)}

  def Definition = rule {"define:" ~ WS.* ~ CapturedWord ~> (w => Tools.Definition(w))}

  def SiteSearch = rule {"site:" ~ WS.* ~ UnlimitedWord ~ SearchQuery ~> ((l, q) => Tools.SiteSearch(l, q))}

  def Translation = rule {
    "translate" ~ WS.* ~ capture((!TranslationEnd ~ ANY).+) ~ TranslationEnd ~>
      ((p, l) => Tools.TranslationQuery(p, l))
  }

  def TranslationEnd = rule {WS.* ~ "into" ~ WS.* ~ CapturedWord ~ EOI}

  def CapturedWord = rule {capture(Word) ~ WS.*}

  def UnlimitedWord = rule {capture((!WS ~ ANY).+) ~ WS.*}
}
