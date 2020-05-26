import sbt.Keys._

ThisBuild / scalaVersion := "2.13.2"

resolvers += Resolver.typesafeIvyRepo("releases")

lazy val root = (project in file(".")).settings(
  name := "ScalaExplore",
  version := "0.1",
  scalaVersion := "2.13.2",
  // libraryDependencies ++= LibDepends.sbtDepends,
  libraryDependencies ++= LibDepends.basicDepends,
).aggregate(s1, chewbacca) // make watchSources work for the sub--projects


lazy val s1 = (project in file("scala1")).settings(
  name := "Scala1",
  version := "0.1",
  scalaVersion := "2.13.2",
  libraryDependencies ++= LibDepends.basicDepends,
  mainClass in(Compile, run) := Some("com.kdr2.scala0.Main")
)

lazy val chewbacca = (project in file("chewbacca")).settings(
  name := "Chewbacca",
  version := "0.1",
  scalaVersion := "2.13.2",
  libraryDependencies ++= LibDepends.basicDepends,
  libraryDependencies ++= LibDepends.akkaDepends
)

Build.hello in s1 := {
  (runMain in s1 in Compile).toTask(" com.kdr2.scala0.MainHello").value
}

Build.hello in root := {
  println("root!")
}
