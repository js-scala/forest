name := "forest"

organization := "forest"

version := "0.1-SNAPSHOT"

scalaHome <<= baseDirectory { dir =>
  val props = new java.util.Properties()
  IO.load(props, dir / "local.properties")
  val x = props.getProperty("scala.virtualized.home")
  if (x == null)
    sys.error("Please set a scala.virtualized.home property in local.properties file")
  else Some(file(x))
}

// scalaVersion := "2.10.0-virtualized-SNAPSHOT"

scalaVersion := "2.10.0-M2-0020-geab3b7e4d7-2012-07-09"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-Yvirtualize")

resolvers ++= Seq(
  ScalaToolsSnapshots,
  "Dropbox" at "http://dl.dropbox.com/u/12870350/scala-virtualized"
)

libraryDependencies ++= Seq(
  "EPFL" % "lms-sandbox_2.10.0-virtualized-SNAPSHOT" % "0.1-SNAPSHOT",
  "scalate" %% "scuery" % "4.0.0-SNAPSHOT",
  "org.scalatest" % "scalatest_2.10.0-virtualized-SNAPSHOT" % "1.6.1-SNAPSHOT" % "test"
)

parallelExecution in Test := false