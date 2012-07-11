name := "compiler"

organization := "forest"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.0-M2-0020-geab3b7e4d7-2012-07-09"

//scalaVersion := "2.10.0-virtualized-SNAPSHOT"

scalaHome <<= baseDirectory { dir =>
  val props = new java.util.Properties()
  IO.load(props, dir / "local.properties")
  val x = props.getProperty("scala.virtualized.home")
  if (x == null)
    sys.error("Please set a scala.virtualized.home property in local.properties file")
  else Some(file(x))
}

mainClass := Some("forest.Run")

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.7.1" % "test",
    "com.github.scala-incubator.io" %% "scala-io-core" % "0.2.0",
    "com.github.scala-incubator.io" %% "scala-io-file" % "0.2.0"
  )

scalacOptions += "-deprecation"
