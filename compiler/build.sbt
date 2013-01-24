name := "forest-compiler"

organization := "js-scala"

version := "0.3-SNAPSHOT"

mainClass := Some("forest.Run")

resolvers += "Sonatype" at "http://oss.sonatype.org/content/repositories/releases"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.12.3" % "test",
    "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.1",
    "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.1"
  )

scalacOptions ++= Seq("-deprecation", "-unchecked")