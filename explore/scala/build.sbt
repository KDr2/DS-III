import sbt.Keys._

lazy val root = project in file(".")


lazy val s1 = (project in file("scala1")).settings(
  name := "Scala1",
  version := "0.1",
  scalaVersion := "2.11.8",
  libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  mainClass in(Compile, run) := Some("com.kdr2.scala0.Main")
)

// scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")

lazy val jst = (project in file("scalajst")).settings(
  name := "ScalaJST",
  version := "0.1",
  scalaVersion := "2.11.8",
  libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.0"
).enablePlugins(ScalaJSPlugin)
