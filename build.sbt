name := "pAut-program"
ThisBuild / version := "0.1.2"

lazy val program = project
  .in(file("."))
  .settings(
    description := "A small library to be used in conjunction with sbt-pAut-plugin that enables easy reading and testing of Advent of Code solutions", 
    console / initialCommands := "import paut.aoc._",
    scalaVersion := "3.2.0",
    crossScalaVersions := List("2.12.16", "2.13.7", "3.2.0"),
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.8.1",
  )
  