
name := "demo"

scalaHome <<= baseDirectory { dir =>
  val props = new java.util.Properties
  IO.load(props, dir / "local.properties")
  val x = props.getProperty("scala.virtualized.home")
  if (x == null) sys.error("Please set a scala.virtualized.home property in local.property file")
  else Some(file(x))
}

scalaVersion := "2.10.0-virtualized-SNAPSHOT"

resolvers += ScalaToolsSnapshots

libraryDependencies += "forest" %% "forest-lms" % "0.1-SNAPSHOT"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-Yvirtualize")

sourceGenerators in Compile <+= (sourceDirectory in Compile, sourceManaged in Compile) map { (sourceDir, targetDir) =>
  forest.Compiler.compile(sourceDir / "forest" / "views" / "Form", targetDir, Seq("app._"), Seq("PersonOps"))
  (targetDir ** "*.scala").get.map(_.getAbsoluteFile)
}

mainClass := Some("app.Run")