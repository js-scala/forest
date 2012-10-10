name := "forest"

organization := "js-scala"

version := "0.3-SNAPSHOT"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.0-M7")

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-Yvirtualize")

resolvers += ScalaToolsSnapshots

libraryDependencies ++= Seq(
  "EPFL" %% "js-scala" % "0.2-SNAPSHOT",
  "EPFL" %% "lms" % "0.3-SNAPSHOT",
  "scalate" %% "scuery" % "4.0.0-SNAPSHOT",
  "org.scalatest" % "scalatest_2.10.0-M7" % "1.9-2.10.0-M7-B1" % "test"
)

parallelExecution in Test := false