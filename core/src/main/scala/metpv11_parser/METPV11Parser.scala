package metpv11_parser

object METPV11Parser {
  // todo エラー処理どうにかする

  /*
  ヘッダー部
  地点コード[5] 地点名[22] スペース[1] 緯度(度)[4] 緯度(分)[6] 経度(度)[4] 経度(分)[6] 標高[6]
  [例1]
  11001 SOYAMISAKI             45 31.2  141 56.1    26.0
  [例2]
  12632 ROKUGO                 43 18.1  142 31.3   315.0
   */

  def parseHeader(str: String) = {
    val (pointCode, t1) = str.splitAt(6)
    val (pointName, t2) = t1.splitAt(22)
    val (lat, t3) = t2.splitAt(4)
    val (latMinute, t4) = t3.splitAt(6)
    val (lon, t5) = t4.splitAt(4)
    val (lonMinute, elevation) = t5.splitAt(6)

    Header(
      pointCode.trim,
      pointName.trim,
      lat.trim.toDouble,
      latMinute.trim.toDouble,
      lon.trim.toDouble,
      lonMinute.trim.toDouble,
      elevation.trim.toDouble)
  }

  /*
  データ部
  データ種別番号[5] スペース[1] 月[2]日[2] 風速計高さ[5] 毎次データ[5]*24 日最大値[5] 日最小値[5] 日積算値[5] 日平均値[5] 日付番号[5]

  [例1]
  00002  1 2  8.0   02   02   02   02   02   02   03   06   06   06   06   06   06   06   06   06   03   02   02   02   02   02   02   02    0 8888    0 8888    2
  [例2]
  00005 1220  8.0 -938 -978-1048-1048-1028-1028-1018-1018-1088-1018-1038 -948 -998 -968 -948 -868 -928 -898 -888 -948 -938 -938 -958 -938  -86 -108 8888  -97  354
  */

  def parseRow(str: String) = {
    val (dataType, t1) = str.splitAt(6)
    val (month, t2) = t1.splitAt(2)
    val (day, t3) = t2.splitAt(2)
    val (anemometerHeight, t4) = t3.splitAt(5)
    val (rowHours, t5) = t4.splitAt(120)
    val hours = parseHours(rowHours)
    val (max, t6) = t5.splitAt(5)
    val (min, t7) = t6.splitAt(5)
    val (accu, t8) = t7.splitAt(5)
    val (avg, t9) = t8.splitAt(5)
    val (dayNumber, t10) = t9.splitAt(5)

    Row(dataType.trim, month.trim.toInt, day.trim.toInt, anemometerHeight.trim.toDouble, hours, max.trim.toDouble, min.trim.toDouble, accu.trim.toDouble, avg.trim.toDouble, dayNumber.trim.toInt)
  }

  def parseHours(str: String) = {
    (0 to 23).foldLeft(Nil: List[Double]) { (ls, n) =>
      val start = n * 5
      str.slice(start, start + 5).trim.toDouble :: ls
    }.reverse
  }

  def parse(str: String) = {
    import scala.collection.mutable.Map

    val head :: tail = str.split("\n").toList
    val header = parseHeader(head)

    // key day number
    val globalSolarRadiations = Map[Int, List[Double]]()
    val directSolarRadiations = Map[Int, List[Double]]()
    val diffuseSolarIrradiances = Map[Int, List[Double]]()
    val daylightHours = Map[Int, List[Double]]()
    val temperatures = Map[Int, List[Double]]()
    val windDirections = Map[Int, List[Double]]()
    val windSpeeds = Map[Int, List[Double]]()
    val precipitations = Map[Int, List[Double]]()
    val snowDepths = Map[Int, List[Double]]()

    val rows = tail.foreach { rowStr =>
      val row = parseRow(rowStr)

      DataType.fromString(row.dataTypeNumber).get match {
        case DataType.GlobalSolarRadiation => {
          globalSolarRadiations.put(row.dayNumber, row.values)
        }
        case DataType.DirectSolarRadiation => {
          directSolarRadiations.put(row.dayNumber, row.values)
        }
        case DataType.DiffuseSolarIrradiance => {
          diffuseSolarIrradiances.put(row.dayNumber, row.values)
        }
        case DataType.DaylightHour => {
          daylightHours.put(row.dayNumber, row.values)
        }
        case DataType.Temperature => {
          temperatures.put(row.dayNumber, row.values)
        }
        case DataType.WindDirection => {
          windDirections.put(row.dayNumber, row.values)
        }
        case DataType.WindSpeed => {
          windSpeeds.put(row.dayNumber, row.values)
        }
        case DataType.Precipitation => {
          precipitations.put(row.dayNumber, row.values)
        }
        case DataType.SnowDepth => {
          snowDepths.put(row.dayNumber, row.values)
        }
      }
    }

    (header, (globalSolarRadiations, directSolarRadiations, diffuseSolarIrradiances, daylightHours, temperatures, windDirections, windSpeeds, precipitations, snowDepths))
  }
}