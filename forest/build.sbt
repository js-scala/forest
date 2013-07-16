name := "forest"

organization := "js-scala"

version := "0.5-SNAPSHOT"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.10.2-RC1"

resolvers += Resolver.sonatypeRepo("snapshots")

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xexperimental", "-Yvirtualize")

resolvers += ScalaToolsSnapshots

libraryDependencies ++= Seq(
  "EPFL" %% "js-scala" % "0.4-SNAPSHOT",
  "EPFL" %% "lms" % "0.3-SNAPSHOT",
  "org.fusesource.scalate" %% "scalate-core" % "1.6.1",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)

parallelExecution in Test := false