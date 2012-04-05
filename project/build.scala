import sbt._
import Keys._

object Forest extends Build {
  lazy val compiler = Project(id = "forest-compiler", base = file("compiler"))
  lazy val lmsRuntime = Project(id = "forest-lms", base = file("lms"))
}