import scalariform.formatter.preferences._

lazy val commonSettings = Seq(
  organization := "com.github.1sh1k4w4-r",
  scalaVersion := "2.12.8",
  scalacOptions ++= Seq("-deprecation"),
  version := "0.1.1",
  githubOwner := "1sh1k4w4-r",
  githubRepository := "metpv11-parser",
  githubTokenSource := TokenSource.GitConfig("github.token")
)

lazy val root = (project in file("."))
  .settings(commonSettings)
  .settings(
    name := "metpv11-parser"
  )

lazy val core = (project in file("core"))
  .settings(commonSettings)
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
