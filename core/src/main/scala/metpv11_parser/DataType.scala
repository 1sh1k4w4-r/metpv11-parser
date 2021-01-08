package metpv11_parser

/*
データ種別番号 項目名（単位は調査中)
00001 水平面全天日射量
00002 水平面直達日射量
00003 水平面天空散乱日射量
00004 日射時間
00005 気温
00006 風向
00007 風速
00008 降水量
00009 積雪深
 */

trait DataType {
  val number: String
  val kana: String
}

object DataType {

  case object GlobalSolarRadiation extends DataType {
    override val number: String = "00001"
    override val kana: String = "水平面全天日射量"
  }

  case object DirectSolarRadiation extends DataType {
    override val number: String = "00002"
    override val kana: String = "水平面直達日射量"
  }

  case object DiffuseSolarIrradiance extends DataType {
    override val number: String = "00003"
    override val kana: String = "水平面天空散乱日射量"
  }

  case object DaylightHour extends DataType {
    override val number: String = "00004"
    override val kana: String = "日射時間"
  }

  case object Temperature extends DataType {
    override val number: String = "00005"
    override val kana: String = "気温"
  }

  case object WindDirection extends DataType {
    override val number: String = "00006"
    override val kana: String = "風向"
  }

  case object WindSpeed extends DataType {
    override val number: String = "00007"
    override val kana: String = "風速"
  }

  case object Precipitation extends DataType {
    override val number: String = "00008"
    override val kana: String = "降水量"
  }

  case object SnowDepth extends DataType {
    override val number: String = "00009"
    override val kana: String = "積雪深"
  }

  def fromString(str: String): Option[DataType] = {
    str match {
      case "00001" => Some(GlobalSolarRadiation)
      case "00002" => Some(DirectSolarRadiation)
      case "00003" => Some(DiffuseSolarIrradiance)
      case "00004" => Some(DaylightHour)
      case "00005" => Some(Temperature)
      case "00006" => Some(WindDirection)
      case "00007" => Some(WindSpeed)
      case "00008" => Some(Precipitation)
      case "00009" => Some(SnowDepth)
      case _ => None
    }
  }

}