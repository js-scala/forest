# Forest: DSL for data representation in HTML

Forest is a DSL to define how to represent data in HTML. It can produce JavaScript or Scala computations, thus allowing to render HTML fragments on client side or on server side.

# Setup

First, setup [js-scala](http://github.com/js-scala/js-scala) and [scuery](http://github.com/js-scala/scuery).

Then, you can build the forest DSL:

Go under the `forest/` subdirectory and run `sbt`. You can check the `test`s, or `publish-local`.

You may also be interested in the external DSL layer:

Go under the `compiler/` subdirectory and run `sbt publish-local`.

See how it is used in the `demo/` build definition.