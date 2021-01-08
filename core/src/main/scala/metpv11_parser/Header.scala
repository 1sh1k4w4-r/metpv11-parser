package metpv11_parser

import scala.util.parsing.combinator.RegexParsers

case class Header(
  pointCode: String,
  pointName: String,
  latitude: Double,
  latitudeLe: Double,
  longitude: Double,
  longitudeLe: Double,
  elevation: Double)

/*
https://www.nedo.go.jp/content/100500502.pdf
データ形式
地点コード[5] 地点名[22] 緯度-度[4]	緯度-分以下[6]	経度-度[4]	経度-分以下(6） 標高m[6]

[例1]
11001 SOYAMISAKI             45 31.2  141 56.1    26.0
[例2]
12632 ROKUGO                 43 18.1  142 31.3   315.0
 */

object Header extends RegexParsers {
  val eol = "\n" | "\\z".r

  val pointCode: Parser[String] = """\d{5}""".r

  val pointName: Parser[String] = """[A-Z]+""".r

  val lat: Parser[Double] = """\d{1,4}""".r ^^ { _.toDouble }

  val latLe: Parser[Double] = """\d{1,3}.\d""".r ^^ { _.toDouble }

  val lon = lat

  val lonLe = latLe

  val elevation: Parser[Double] = """\d{1,4}.\d""".r ^^ { _.toDouble }

  val value: Parser[String ~ String ~ Double ~ Double ~ Double ~ Double ~ Double] = pointCode ~ pointName ~ lat ~ latLe ~ lon ~ lonLe ~ elevation <~ opt(eol)

  def parse(input: String): Option[Header] = {

    val parser = value ^^ {
      case pointCode ~ pointName ~ lat ~ latLe ~ lon ~ lonLe ~ elevation =>
        Header(
          pointCode,
          pointName,
          lat,
          latLe,
          lon,
          lonLe,
          elevation)
    }

    parseAll(parser, input) match {
      case Success(result, _) => Some(result)
      case _ => None
    }
  }
}