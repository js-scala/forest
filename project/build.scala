import sbt._
import Keys._

object Forest extends Build {
  lazy val compiler = Project(id = "forest-compiler", base = file("compiler"))
  lazy val playRuntime = Project(id = "forest-play-runtime", base = file("play-runtime")) dependsOn compiler
  lazy val root = Project(id = "forest", base = file(".")) aggregate (compiler, playRuntime)
}