ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    name := "scala_funtional_programing_v1"

  )
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
