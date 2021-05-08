package metpv11_parser

case class Row(
  dataTypeNumber: String,
  month: Int,
  day: Int,
  anemometerHeight: Double,
  values: List[Double],
  max: Double,
  min: Double,
  accum: Double,
  average: Double,
  dayNumber: Int)