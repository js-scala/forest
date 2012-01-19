name := "forest"

mainClass := Some("forest.Run")

version := "0.1"

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.6.1" % "test",
    "com.github.scala-incubator.io" %% "scala-io-core" % "0.3.0",
    "com.github.scala-incubator.io" %% "scala-io-file" % "0.3.0"
  )

scalacOptions += "-deprecation"