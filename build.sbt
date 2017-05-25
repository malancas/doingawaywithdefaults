import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "doingaway",
      scalaVersion := "2.12.2",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "DoingAwayWithDefaults",
    resolvers += "scala-csv" at "https://github.com/tototoshi/scala-csv"
    libraryDependencies += scalaTest % Test
    libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.4"
  )
