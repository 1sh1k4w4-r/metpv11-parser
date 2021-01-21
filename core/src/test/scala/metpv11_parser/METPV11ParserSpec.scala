package metpv11_parser

import org.scalatest.FlatSpec

import scala.io.Source

class METPV11ParserSpec extends FlatSpec {

  it should "parse header" in {
    {
      val input = "11016 WAKKANAI               45 24.9  141 40.7     3.0"

      val res = METPV11Parser.parse(METPV11Parser.header, input)
      val spec = Header(
        "11016",
        "WAKKANAI",
        45.0,
        24.9,
        141.0,
        40.7,
        3.0)

      assert(res.successful && res.get == spec)
    }

    {
      // 地点名にハイフンを含むケース
      val input = "41247 NASU-KARASUYAMA        36 38.5  140  7.0    82.0"

      val res = METPV11Parser.parse(METPV11Parser.header, input)
      val spec = Header(
        "41247",
        "NASU-KARASUYAMA",
        36.0,
        38.5,
        140.0,
        7.0,
        82.0)

      assert(res.successful && res.get == spec)
    }

    {
      // 標高がマイナスになるケース
      val input = "32287 OGATA                  40  0.0  139 57.0    -3.0"

      val res = METPV11Parser.parse(METPV11Parser.header, input)
      val spec = Header(
        "32287",
        "OGATA",
        40.0,
        0.0,
        139.0,
        57.0,
        -3.0)
      assert(res.successful && res.get == spec)
    }
  }

  it should "parse month day" in {
    val input1 = "1231"
    val input2 = " 1 1"

    val res1 = METPV11Parser.parse(METPV11Parser.monthDay, input1)
    val res2 = METPV11Parser.parse(METPV11Parser.monthDay, input2)

    assert(res1.successful && res1.get == (12, 31))
    assert(res2.successful && res2.get == (1, 1))
  }

  it should "parse signedFiveInteger" in {
    val inputs = List(
      "   02",
      "  -22",
      " -858",
      "-1234",
      "12345")

    val results = inputs.map { input => METPV11Parser.parseAll(METPV11Parser.signedFiveInteger, input) }

    results.foreach { result => assert(result.successful) }
    assert(results.map(_.get) == List(2, -22, -858, -1234, 12345))
  }

  it should "parse hours data" in {
    {
      val input = """   02   02   02   02   02   02   06   76  446  756 1016 1056  996  766  476  106   06   02   02   02   02   02   02   02"""

      val res = METPV11Parser.parse(METPV11Parser.hours, input)
      val spec = List(2, 2, 2, 2, 2, 2, 6, 76, 446, 756, 1016, 1056, 996, 766, 476, 106, 6, 2, 2, 2, 2, 2, 2, 2)

      assert(res.successful && res.get.length == 24)
      assert(res.get == spec)
    }

    {
      val input = """   08  -28   08  -38  -38   08  -18   28   78  118  148  178  188  218  198  188  208  178   88   38  -38  -48  -68  -68"""

      val res = METPV11Parser.parse(METPV11Parser.hours, input)
      val spec = List(8, -28, 8, -38, -38, 8, -18, 28, 78, 118, 148, 178, 188, 218, 198, 188, 208, 178, 88, 38, -38, -48, -68, -68)

      assert(res.successful && res.get.length == 24)
      assert(res.get == spec)
    }

    {
      val input = """   08   08   08   58  408  208  108  108   08  108  408  408  758  408  958 1558 500812558 6808 4258   08   58   08   08"""

      val res = METPV11Parser.parse(METPV11Parser.hours, input)
      val spec = List(8, 8, 8, 58, 408, 208, 108, 108, 8, 108, 408, 408, 758, 408, 958, 1558, 5008, 12558, 6808, 4258, 8, 58, 8, 8)

      assert(res.successful && res.get.length == 24)
      assert(res.get == spec)
    }
  }

  it should "parse daily data" in {
    {
      val input =
        """00001  2 6  8.0   02   02   02   02   02   02   06  266  746 1226 1556 1436 1666  996  426  236   46   06   02   02   02   02   02   02  166 8888  854 8888   37
00002  2 6  8.0   02   02   02   02   02   02   03  106  466  846 1116  616 1206  236   06   06   06   03   02   02   02   02   02   02  120 8888  455 8888   37
00003  2 6  8.0   02   02   02   02   02   02   03  166  286  386  446  826  466  766  426  236   46   03   02   02   02   02   02   02   82 8888  399 8888   37
00004  2 6  8.0   02   02   02   02   02   02   07   77  107  107  107   77  107   47   07   07   07   02   02   02   02   02   02   02   10 8888   58 8888   37
00005  2 6  8.0 -758 -778 -798 -858-1048-1038-1048 -908 -838 -698 -618 -588 -628 -648 -628 -498 -458 -428 -418 -398 -378 -378 -328 -368  -32 -104 8888  -64   37
00006  2 6  8.0   18   18   38   38   88   88   88   88   88   88   98  108  108  108   98  138  128  138  138  138  138  128  138  158   13 8888 8888 8888   37
00007  2 6  8.0  308  208  208  108  508  508  508  508  408  408  208  308  308  408  608  808  708  708  708  708  608  508  408  508   80 8888 8888   46   37
00008  2 6  8.0   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08    0 8888    0 8888   37
00009  2 6  8.0  225  215  215  215  215  215  215  215  215  215  215  215  215  235  235  235  235  235  235  235  235  235  235  235   23 8888 8888 8888   37
"""

      val res = METPV11Parser.parse(METPV11Parser.day, input)
      assert(res.successful)
    }

    {
      // データの欠損
      val input =
        """00001  1 1  8.0   02   02   02   02   02   02   06   76  446  756 1016 1056  996  766  476  106   06   02   02   02   02   02   02   02  105 8888  564 8888    1
00002  1 1  8.0   02   02   02   02   02   02   03   26  256  436  626  556  516  386  236   26   03   02   02   02   02   02   02   02   62 8888  301 8888    1
00003  1 1  8.0   02   02   02   02   02   02   03   56  196  326  396  506  486  386  246   86   03   02   02   02   02   02   02   02   50 8888  263 8888    1
00004  1 1  8.0   02   02   02   02   02   02   02   48  108  108  108   98   98   98   98   38   02   02   02   02   02   02   02   02   10 8888   73 8888    1
00006  1 1  8.0  128  128  128  138  128  128  128  118  128  118  128  118  118  118  118  128  128  138  138  148  158  148  148  148   12 8888 8888 8888    1
00007  1 1  8.0 1208 1108 1008 1008  908 1008 1308 1008 1208 1108 1208  908 1008 1008  908  808  908 1108 1108  808  708  708  608  608  130 8888 8888   96    1
00008  1 1  8.0   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08    0 8888    0 8888    1
00009  1 1  8.0   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45    4 8888 8888 8888    1
"""

      val res = METPV11Parser.parse(METPV11Parser.day, input)
      assert(res.successful == false)
    }
  }

  it should "parse" in {
    val input = Source.fromResource("max11001.txt").getLines().mkString("\n")

    val res = METPV11Parser.parse(input)

    assert(res.isRight)

    val (header, data) = res.right.get

    assert(data.length == 365)
  }
}
