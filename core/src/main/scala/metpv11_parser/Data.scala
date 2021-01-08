package metpv11_parser

import scala.util.parsing.combinator.RegexParsers

case class Raw(
  dataTypeNumber: String,
  month: Int,
  day: Int,
  anemometerHeight: Double,
  values: List[Int],
  max: Int,
  min: Int,
  accum: Int,
  average: Int,
  dayNumber: Int)

case class Data(
  month: Int,
  day: Int,
  // 00001
  globalSolarRadiations: List[Int],
  maxGlobalSolarRadiation: Int,
  accumGlobalSolarRadiation: Int,
  // 00002
  directSolarRadiations: List[Int],
  maxDirectSolarRadiation: Int,
  accumDirectSolarRadiation: Int,
  // 00003
  diffuseSolarIrradiances: List[Int],
  maxDiffuseSolarIrradiance: Int,
  accumDiffuseSolarIrradiances: Int,
  // 00004
  daylightHours: List[Int],
  maxDaylightHour: Int,
  accumDaylightHour: Int,
  // 00005
  temperatures: List[Int],
  maxTemperature: Int,
  minTemperature: Int,
  averageTemperature: Int,
  // 00006
  windDirections: List[Int],
  maxWindDirection: Int,
  // 00007
  windSpeeds: List[Int],
  maxWindSpeed: Int,
  averageWindSpeed: Int,
  // 00008
  precipitations: List[Int],
  maxPrecipitation: Int,
  accumPrecipitation: Int,
  // 00009
  snowDepths: List[Int],
  maxSnowDepth: Int,
  dayNumber: Int)

/*
https://www.nedo.go.jp/content/100500502.pdf

データの形式
データ種別番号[5] 月[2]日[2] 風速計高さ[5] 毎次データ[4]*24 日最大値[4] 日最小値[4] 日積算値[4] 日平均値[4] 日付番号[4]

[例1]
00002  1 2  8.0   02   02   02   02   02   02   03   06   06   06   06   06   06   06   06   06   03   02   02   02   02   02   02   02    0 8888    0 8888    2
[例2]
00005 1220  8.0 -938 -978-1048-1048-1028-1028-1018-1018-1088-1018-1038 -948 -998 -968 -948 -868 -928 -898 -888 -948 -938 -938 -958 -938  -86 -108 8888  -97  354
 */

object Data extends RegexParsers {
  val eol = "\n" | "\\z".r

  val twoDigits: Parser[Int] =
    """\d{1,2}""".r ^^ {
      _.toInt
    }

  val monthDay: Parser[(Int, Int)] = twoDigits ~ twoDigits ^^ { case month ~ day => (month, day) }

  val fourDigits: Parser[Int] =
    """-*\d{1,4}""".r ^^ {
      _.toInt
    }

  val anemometerHeight: Parser[Double] =
    """\d{1,2}.\d""".r ^^ {
      _.toDouble
    }

  val hours: Parser[List[Int]] = repN(24, fourDigits)

  def row(dateType: DataType): Parser[Raw] = {
    dateType.number ~ monthDay ~ anemometerHeight ~ hours ~ fourDigits ~ fourDigits ~ fourDigits ~ fourDigits ~ fourDigits <~ opt(eol) ^^ {
      case dataType ~ monthDay ~ anemometerHeight ~ hours ~ max ~ min ~ accu ~ avg ~ dayNumber =>
        Raw(
          dataType,
          monthDay._1,
          monthDay._2,
          anemometerHeight,
          hours,
          max,
          min,
          accu,
          avg,
          dayNumber)
    }
  }

  val day: Parser[Data] = row(DataType.GlobalSolarRadiation) ~
    row(DataType.DirectSolarRadiation) ~
    row(DataType.DiffuseSolarIrradiance) ~
    row(DataType.DaylightHour) ~
    row(DataType.Temperature) ~
    row(DataType.WindDirection) ~
    row(DataType.WindSpeed) ~
    row(DataType.Precipitation) ~
    row(DataType.SnowDepth) ^^ {
      case gsr ~ dsr ~ dsi ~ dh ~ temp ~ wd ~ ws ~ p ~ sd =>
        Data(
          month = gsr.month,
          day = gsr.day,
          globalSolarRadiations = gsr.values,
          maxGlobalSolarRadiation = gsr.max,
          accumGlobalSolarRadiation = gsr.accum,
          directSolarRadiations = dsr.values,
          maxDirectSolarRadiation = dsr.max,
          accumDirectSolarRadiation = dsr.accum,
          diffuseSolarIrradiances = dsi.values,
          maxDiffuseSolarIrradiance = dsi.max,
          accumDiffuseSolarIrradiances = dsi.accum,
          daylightHours = dh.values,
          maxDaylightHour = dh.max,
          accumDaylightHour = dh.accum,
          temperatures = temp.values,
          maxTemperature = temp.max,
          minTemperature = temp.min,
          averageTemperature = temp.average,
          windDirections = wd.values,
          maxWindDirection = wd.max,
          windSpeeds = ws.values,
          maxWindSpeed = ws.max,
          averageWindSpeed = ws.average,
          precipitations = p.values,
          maxPrecipitation = p.max,
          accumPrecipitation = p.accum,
          snowDepths = sd.values,
          maxSnowDepth = sd.max,
          dayNumber = gsr.dayNumber)
    }

  val value: Parser[List[Data]] = repN(365, day)

  def parse(input: String): Option[List[Data]] = {
    parseAll(value, input) match {
      case Success(result, _) => Some(result)
      case _ => None
    }
  }

}
