name := "compiler"

organization := "forest"

version := "0.2-SNAPSHOT"

scalaVersion := Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.0-M1-virtualized")

mainClass := Some("forest.Run")

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.9" % "test",
    "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.0",
    "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.0"
  )

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-Yvirtualize")