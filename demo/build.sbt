
name := "demo"

scalaVersion := Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.0-M1-virtualized")

resolvers += ScalaToolsSnapshots

libraryDependencies += "forest" %% "forest" % "0.3-SNAPSHOT"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-Yvirtualize")

sourceGenerators in Compile <+= (sourceDirectory in Compile, sourceManaged in Compile) map { (sourceDir, targetDir) =>
  forest.compiler.Compiler.compile(scalax.file.Path(sourceDir / "forest" / "views" / "Form"), scalax.file.Path(targetDir), Seq("app._"), Seq("PersonOps"))
  (targetDir ** "*.scala").get.map(_.getAbsoluteFile)
}

mainClass := Some("app.Run")