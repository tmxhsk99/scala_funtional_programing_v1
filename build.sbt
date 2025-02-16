ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

// build.sbt
val catsVersion = "2.10.0"  // 최신 안정 버전

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-free" % catsVersion

)
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.4"  // 최신 안정 버전


// build.sbt
libraryDependencies ++= Seq(
  "org.tpolecat" %% "doobie-core" % "1.0.0-RC1",
  "org.tpolecat" %% "doobie-postgres" % "1.0.0-RC1"
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.5.2",
  "org.http4s" %% "http4s-dsl" % "0.23.24",
  "org.http4s" %% "http4s-ember-client" % "0.23.24"
)
lazy val root = (project in file("."))
  .settings(
    name := "scala_funtional_programing_v1"

  )
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test

