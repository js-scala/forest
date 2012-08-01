name := "forest-compiler"

organization := "js-scala"

version := "0.3-SNAPSHOT"

mainClass := Some("forest.Run")

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.9" % "test",
    "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.0",
    "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.0"
  )

scalacOptions ++= Seq("-deprecation", "-unchecked")