name := "forest-lms"

version := "0.1"

scalaHome <<= baseDirectory { dir =>
  val props = new java.util.Properties()
  IO.load(props, dir / "local.properties")
  val x = props.getProperty("scala.virtualized.home")
  if (x == null)
    sys.error("Please set a scala.virtualized.home property in local.properties file")
  else Some(file(x))
}

scalaVersion := "2.10.0-virtualized-SNAPSHOT"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-Yvirtualize")

resolvers += ScalaToolsSnapshots

libraryDependencies += "EPFL" % "lms-sandbox_2.10.0-virtualized-SNAPSHOT" % "0.1-SNAPSHOT"