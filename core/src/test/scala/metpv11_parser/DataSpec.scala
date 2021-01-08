package metpv11_parser

import org.scalatest.FlatSpec

class DataSpec extends FlatSpec {

  it should "parse month day" in {
    val input1 = "1231"
    val input2 = " 1 1"

    val res1 = Data.parse(Data.monthDay, input1)
    val res2 = Data.parse(Data.monthDay, input2)

    assert(res1.successful && res1.get == (12, 31))
    assert(res2.successful && res2.get == (1, 1))

  }

  it should "parse digits" in {
    val input1 = "  02"
    val input2 = " 215"
    val input3 = "1016"
    val input4 = "-13"

    val res1 = Data.parse(Data.fourDigits, input1)
    val res2 = Data.parse(Data.fourDigits, input2)
    val res3 = Data.parse(Data.fourDigits, input3)
    val res4 = Data.parse(Data.fourDigits, input4)

    assert(res1.successful && res1.get == 2)
    assert(res2.successful && res2.get == 215)
    assert(res3.successful && res3.get == 1016)
    assert(res4.successful && res4.get == -13)
  }

  it should "parse hours data" in {
    val input1 = "  02   02   02   02   02   02   06   76  446  756 1016 1056  996  766  476  106   06   02   02   02   02   02   02   02"

    val res1 = Data.parse(Data.hours, input1)
    val spec1 = List(2, 2, 2, 2, 2, 2, 6, 76, 446, 756, 1016, 1056, 996, 766, 476, 106, 6, 2, 2, 2, 2, 2, 2, 2)

    assert(res1.successful && res1.get.length == 24)
    assert(res1.get == spec1)

    val input2 = "08  -28   08  -38  -38   08  -18   28   78  118  148  178  188  218  198  188  208  178   88   38  -38  -48  -68  -68"

    val res2 = Data.parse(Data.hours, input2)
    val spec2 = List(8, -28, 8, -38, -38, 8, -18, 28, 78, 118, 148, 178, 188, 218, 198, 188, 208, 178, 88, 38, -38, -48, -68, -68)

    assert(res2.successful && res2.get.length == 24)
    assert(res2.get == spec2)
  }

  it should "parse daily data" in {
    val input1 =
      """
00001  1 1  8.0   02   02   02   02   02   02   06   76  446  756 1016 1056  996  766  476  106   06   02   02   02   02   02   02   02  105 8888  564 8888    1
00002  1 1  8.0   02   02   02   02   02   02   03   26  256  436  626  556  516  386  236   26   03   02   02   02   02   02   02   02   62 8888  301 8888    1
00003  1 1  8.0   02   02   02   02   02   02   03   56  196  326  396  506  486  386  246   86   03   02   02   02   02   02   02   02   50 8888  263 8888    1
00004  1 1  8.0   02   02   02   02   02   02   02   48  108  108  108   98   98   98   98   38   02   02   02   02   02   02   02   02   10 8888   73 8888    1
00005  1 1  8.0   08  -28   08  -38  -38   08  -18   28   78  118  148  178  188  218  198  188  208  178   88   38  -38  -48  -68  -68   21   -6 8888    6    1
00006  1 1  8.0  128  128  128  138  128  128  128  118  128  118  128  118  118  118  118  128  128  138  138  148  158  148  148  148   12 8888 8888 8888    1
00007  1 1  8.0 1208 1108 1008 1008  908 1008 1308 1008 1208 1108 1208  908 1008 1008  908  808  908 1108 1108  808  708  708  608  608  130 8888 8888   96    1
00008  1 1  8.0   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08    0 8888    0 8888    1
00009  1 1  8.0   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45    4 8888 8888 8888    1
"""

    val res1 = Data.parse(Data.day, input1)

    assert(res1.successful)

    // データの欠損
    val input2 =
      """
00001  1 1  8.0   02   02   02   02   02   02   06   76  446  756 1016 1056  996  766  476  106   06   02   02   02   02   02   02   02  105 8888  564 8888    1
00002  1 1  8.0   02   02   02   02   02   02   03   26  256  436  626  556  516  386  236   26   03   02   02   02   02   02   02   02   62 8888  301 8888    1
00003  1 1  8.0   02   02   02   02   02   02   03   56  196  326  396  506  486  386  246   86   03   02   02   02   02   02   02   02   50 8888  263 8888    1
00004  1 1  8.0   02   02   02   02   02   02   02   48  108  108  108   98   98   98   98   38   02   02   02   02   02   02   02   02   10 8888   73 8888    1
00006  1 1  8.0  128  128  128  138  128  128  128  118  128  118  128  118  118  118  118  128  128  138  138  148  158  148  148  148   12 8888 8888 8888    1
00007  1 1  8.0 1208 1108 1008 1008  908 1008 1308 1008 1208 1108 1208  908 1008 1008  908  808  908 1108 1108  808  708  708  608  608  130 8888 8888   96    1
00008  1 1  8.0   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08   08    0 8888    0 8888    1
00009  1 1  8.0   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45   45    4 8888 8888 8888    1
"""

    val res2 = Data.parse(Data.day, input2)

    assert(res2.successful == false)

  }
}
