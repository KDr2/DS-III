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
    "org.scala-lang" % "scala-compiler" % "2.11.8",
    "org.scala-lang" % "scala-library" % "2.11.8",
    "org.scala-lang" % "scala-reflect" % "2.11.8",
    "org.scala-lang.modules" %% "scala-xml" % "1.0.4",
    "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
  ))

  val sbtDepends = libs(Seq(
    "main",
    "interface",
    "compiler-interface",
    "compiler-integration",
    "sbt"
  ).map("org.scala-sbt" %% _ % "0.13.11"))

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
    "akka-distributed-data-experimental",
    "akka-typed-experimental",
    "akka-http-experimental",
    "akka-http-jackson-experimental",
    "akka-http-spray-json-experimental",
    "akka-http-xml-experimental",
    "akka-persistence-query-experimental"
  ).map("com.typesafe.akka" %% _ % "2.4.9"))
}
