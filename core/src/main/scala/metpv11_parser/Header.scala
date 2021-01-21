package metpv11_parser

case class Header(
  pointCode: String, // 地点コード
  pointName: String, // 地点名
  latitude: Double, // 緯度(度)
  latitudeMinute: Double, // 緯度(分)
  longitude: Double, // 経度(度)
  longitudeMinute: Double, // 経度(分)
  elevation: Double // 標高(m)
)