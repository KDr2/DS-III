import sbt.Keys._

resolvers += Resolver.typesafeIvyRepo("releases")

val basicDepends = Build.libs(Seq(
  "org.scala-lang" % "scala-compiler" % "2.11.8",
  "org.scala-lang" % "scala-library" % "2.11.8",
  "org.scala-lang" % "scala-reflect" % "2.11.8",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.4",
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
))

val sbtDepends = Build.libs(Seq(
  "org.scala-sbt" % "main" % "0.13.11",
  "org.scala-sbt" % "interface" % "0.13.11",
  "org.scala-sbt" % "compiler-interface" % "0.13.11",
  "org.scala-sbt" % "compiler-integration" % "0.13.11",
  "org.scala-sbt" % "sbt" % "0.13.11"
))


lazy val root = (project in file(".")).settings(
  name := "ScalaExplore",
  version := "0.1",
  scalaVersion := "2.11.8",
  libraryDependencies ++= sbtDepends,
  libraryDependencies ++= basicDepends
)


lazy val s1 = (project in file("scala1")).settings(
  name := "Scala1",
  version := "0.1",
  scalaVersion := "2.11.8",
  libraryDependencies ++= basicDepends,
  mainClass in(Compile, run) := Some("com.kdr2.scala0.Main")
)

lazy val jst = (project in file("scalajst")).settings(
  name := "ScalaJST",
  version := "0.1",
  scalaVersion := "2.11.8",
  libraryDependencies ++= Build.libs(Seq("org.scala-js" %%% "scalajs-dom" % "0.9.0")),
  libraryDependencies ++= basicDepends
).enablePlugins(ScalaJSPlugin)

Build.hello in s1 := {
  (runMain in s1 in Compile).toTask(" com.kdr2.scala0.MainHello").value
}

Build.hello in jst := {
  println((name in jst).value)
  println("Hello!")
}

Build.hello in root := {
  println("root!")
}