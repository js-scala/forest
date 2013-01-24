import sbt._
import Keys._

object ForestBuild extends Build {

  val defaultSettings = Seq(
    scalaVersion := "2.10.0",
    scalaOrganization := "org.scala-lang.virtualized"
  )

  val forest = Project(id = "forest", base = file("forest")).settings(defaultSettings: _*)

  val compiler = Project(id = "forest-compiler", base = file("compiler")).settings(defaultSettings: _*)

  val root = Project(id = "forest-project", base = file(".")).settings(defaultSettings: _*) aggregate (forest, compiler)

}