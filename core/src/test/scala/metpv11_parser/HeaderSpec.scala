package metpv11_parser

import org.scalatest.FlatSpec

class HeaderSpec extends FlatSpec {
  it should "parse" in {
    {
      val input = "11016 WAKKANAI               45 24.9  141 40.7     3.0"

      val res = Header.parse(input)
      val spec = Header(
        "11016",
        "WAKKANAI",
        45.0,
        24.9,
        141.0,
        40.7,
        3.0)

      assert(res.isDefined)
      assert(res.get == spec)
    }

    {
      // 地点名にハイフンを含むケース
      val input = "41247 NASU-KARASUYAMA        36 38.5  140  7.0    82.0"

      val res = Header.parse(input)
      val spec = Header(
        "41247",
        "NASU-KARASUYAMA",
        36.0,
        38.5,
        140.0,
        7.0,
        82.0)

      assert(res.isDefined)
      assert(res.get == spec)
    }

    {
      // 標高がマイナスになるケース
      val input = "32287 OGATA                  40  0.0  139 57.0    -3.0"

      val res = Header.parse(input)
      val spec = Header(
        "32287",
        "OGATA",
        40.0,
        0.0,
        139.0,
        57.0,
        -3.0)

      assert(res.isDefined)
      assert(res.get == spec)
    }

  }
}
