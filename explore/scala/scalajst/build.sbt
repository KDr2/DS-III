import sbt.Keys._

lazy val root = (project in file(".")).settings(
  name := "ScalaJST",
  version := "0.1",
  scalaVersion := "2.11.7",
  libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
).enablePlugins(ScalaJSPlugin)


// scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")

