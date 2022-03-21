import sbt._

object Build {
  lazy val hello = taskKey[Unit]("An example task")
  lazy val ds3webJS = taskKey[Unit]("Copy javascripts to ds3-web")
}
