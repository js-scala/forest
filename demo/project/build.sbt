resolvers += Resolver.url("ivy-local", url("file://" + Path.userHome + "/.ivy2/local"))(Resolver.ivyStylePatterns)

libraryDependencies += "js-scala" %% "forest-compiler" % "0.3-SNAPSHOT"