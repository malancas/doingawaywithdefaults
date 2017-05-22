import Dependencies._

organization := "doingaway"

scalaVersion := "2.12.2"

version      := "0.1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  scalaTest % Test,
  "com.github.tototoshi" %% "scala-csv" % "1.3.4"
)
