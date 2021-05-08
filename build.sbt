import scalariform.formatter.preferences._

lazy val commonSettings = Seq(
  organization := "com.github.ishikawawawa",
  scalaVersion := "2.12.8",
  scalacOptions ++= Seq("-deprecation"),
  version := "0.1.0",
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

lazy val example = (project in file("example"))
  .settings(commonSettings)
  .settings(
    name := "metpv11-parser-example"
  )
  .dependsOn(core)

scalariformPreferences := scalariformPreferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentConstructorArguments, true)
  .setPreference(DanglingCloseParenthesis, Preserve)
