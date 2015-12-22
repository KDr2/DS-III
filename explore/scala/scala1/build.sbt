name := "Scala1"

version := "0.1"

scalaVersion := "2.11.0"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

mainClass in (Compile, run) := Some("com.kdr2.scala0.Main")

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")
