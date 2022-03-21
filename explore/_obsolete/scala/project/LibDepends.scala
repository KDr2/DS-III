import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbt._

object LibDepends {

  def libs(modules: Seq[ModuleID]): Seq[ModuleID] = {
    if (System.getenv("ENV") == null || System.getenv("ENV") == "DEV") {
      modules.map(_.withSources())
    } else {
      modules
    }
  }

  val basicDepends = libs(Seq(
    "org.scala-lang" % "scala-compiler" % "2.13.2",
    "org.scala-lang" % "scala-library" % "2.13.2",
    "org.scala-lang" % "scala-reflect" % "2.13.2",
    "org.scala-lang.modules" %% "scala-xml" % "latest.integration",
    "org.scalatest" % "scalatest_2.13" % "3.1.2" % "test",
  ))

  val sbtDepends = libs(Seq(
    "main",
    "compiler-interface",
    "sbt"
  ).map("org.scala-sbt" % _ % "1.3.4"))

  val akkaDepends = libs(Seq(
    "akka-actor",
    "akka-agent",
    "akka-camel",
    "akka-cluster",
    "akka-cluster-metrics",
    "akka-cluster-sharding",
    "akka-cluster-tools",
    "akka-contrib",
    "akka-http-core",
    "akka-http-testkit",
    "akka-multi-node-testkit",
    "akka-osgi",
    "akka-persistence",
    "akka-persistence-tck",
    "akka-remote",
    "akka-slf4j",
    "akka-stream",
    "akka-stream-testkit",
    "akka-testkit",
  ).map("com.typesafe.akka" %% _ % "latest.integration"))
}
