package example

import java.nio.file.{ Files, Path }

trait ErrorType {
  val message: String
}

object ErrorType {

  case class NotDirectory(path: Path) extends ErrorType {
    override val message: String = s"${path.normalize}はディレクトリではありません。"
  }

  case class NotExists(path: Path) extends ErrorType {
    override val message: String = s"${path.normalize}が存在しません。"
  }

}

object FileUtils {
  def existsDirectory(path: Path): Either[ErrorType, Path] = {
    if (Files.exists(path)) {
      if (Files.isDirectory(path)) {
        Right(path)
      } else {
        Left(ErrorType.NotDirectory(path))
      }
    } else {
      Left(ErrorType.NotExists(path))
    }
  }
}
