package example

import metpv11_parser.METPV11Parser

import java.io.File
import java.nio.file.Paths
import scala.io.Source

object Example extends App {
  val metpv11Dir = args.lift(0)

  metpv11Dir.fold {
    println("第1引数にMETPV-11データベースのディレクトリを指定してください。")
  } { dirname =>

    FileUtils.existsDirectory(Paths.get(dirname)) match {
      case Left(error) => println(error.message)
      case Right(basePath) => {
        List("平均年", "寡照年", "多照年").foreach { t =>
          FileUtils.existsDirectory(Paths.get(basePath.toString, t)) match {
            case Left(error) => println(error.message)
            case Right(maxPath) => {
              val files = new File(maxPath.toUri).listFiles.toList
              files.foreach { file =>
                println(file.getName)
                try {
                  val res = METPV11Parser.parse(Source.fromFile(file).getLines().mkString("\n"))
                } catch {
                  case e: Exception => {
                    println(s"${file.toPath.normalize.toString} => 失敗(${e.getMessage})")
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}