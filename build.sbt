import scalariform.formatter.preferences._

lazy val commonSetings = Seq(
  organization := "com.github.1sh1k4w4-r",
  scalaVersion := "2.12.8",
  scalacOptions ++= Seq("-deprecation"),
  version := "0.1.0"
)

lazy val publishSettings = Seq(
  githubOwner := "1sh1k4w4-r",
  githubRepository := "metpv11-parser",
  githubTokenSource := TokenSource.GitConfig("github.token")
)

lazy val root = (project in file("."))
  .settings(commonSetings)
  .settings(
    name := "metpv11-parser"
  )

lazy val core = (project in file("core"))
  .settings(commonSetings)
  .settings(publishSettings)
  .settings(
    name := "metpv11-parser-core",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  )

scalariformPreferences := scalariformPreferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentConstructorArguments, true)
  .setPreference(DanglingCloseParenthesis, Preserve)
