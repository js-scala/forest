resolvers += Resolver.url("ivy-local", url("file://" + Path.userHome + "/.ivy2/local"))(Resolver.ivyStylePatterns)

libraryDependencies += "forest" %% "compiler" % "0.2-SNAPSHOT"