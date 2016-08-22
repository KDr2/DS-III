import sbt._

object Build {
  lazy val hello = taskKey[Unit]("An example task")

  def libs(modules: Seq[ModuleID]): Seq[ModuleID] = {
    if (System.getenv("ENV") == null || System.getenv("ENV") == "DEV") {
      modules.map(_.withSources())
    } else {
      modules
    }
  }
}