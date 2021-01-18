package metpv11_parser

case class Header(
  pointCode: String,
  pointName: String,
  latitude: Double,
  latitudeLe: Double,
  longitude: Double,
  longitudeLe: Double,
  elevation: Double)