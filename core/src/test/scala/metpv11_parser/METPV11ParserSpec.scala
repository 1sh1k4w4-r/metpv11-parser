package metpv11_parser

import org.scalatest.FlatSpec

import scala.io.Source

class METPV11ParserSpec extends FlatSpec {

  it should "parse" in {
    val input = Source.fromResource("max11001.txt").getLines().toList

    val res = METPV11Parser.fromString(input)

    assert(res.isDefined)

    val (header, data) = res.get

    assert(data.length == 365)
  }
}
