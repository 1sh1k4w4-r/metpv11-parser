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
        FileUtils.existsDirectory(Paths.get(basePath.toString, "多照年")) match {
          case Left(error) => println(error.message)
          case Right(maxPath) => {
            val files = new File(maxPath.toUri).listFiles.toList

            files.foreach { file =>
              print(s"${file.toPath.normalize.toString} => ")
              METPV11Parser.fromString(Source.fromFile(file).getLines.toList).fold {
                println("失敗")
              } { _ =>
                println("成功")
              }
            }
          }
        }
      }
    }
  }
}