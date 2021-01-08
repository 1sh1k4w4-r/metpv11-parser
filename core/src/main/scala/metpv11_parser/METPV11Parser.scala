package metpv11_parser

object METPV11Parser {
  def fromString(lines: List[String]): Option[(Header, List[Data])] = {
    val head :: tail = lines

    for {
      header <- Header.parse(head)
      data <- Data.parse(tail.mkString("\n"))
    } yield {
      (header, data)
    }
  }

}