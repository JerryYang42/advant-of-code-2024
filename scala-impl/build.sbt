ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.1"

lazy val root = (project in file("."))
  .settings(
    name := "scala-impl",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.16" % Test
  )
