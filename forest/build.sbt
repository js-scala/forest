name := "forest"

organization := "js-scala"

version := "0.3-SNAPSHOT"

scalaVersion := Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.0-M1-virtualized")

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-Yvirtualize")

resolvers ++= Seq(
  ScalaToolsSnapshots,
  "Dropbox" at "http://dl.dropbox.com/u/12870350/scala-virtualized"
)

libraryDependencies ++= Seq(
  "EPFL" %% "js-scala" % "0.2-SNAPSHOT",
  "EPFL" %% "lms" % "0.3-SNAPSHOT",
  "scalate" %% "scuery" % "4.0.0-SNAPSHOT",
  "org.scalatest" % "scalatest_2.10.0-virtualized-SNAPSHOT" % "1.6.1-SNAPSHOT" % "test"
)

parallelExecution in Test := false