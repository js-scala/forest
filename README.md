# Forest: DSL for defining HTML fragments

Forest is a DSL to define how to represent data in HTML. It can produce JavaScript or Scala computations, thus allowing to render HTML fragments on client side or on server side.

Example of HTML template using forest:

```scala
def profile(user: Rep[User]) =
  el('dl)(
    el('dt)("Name"),
    el('dd)(user.name),
    el('dt)("Age"),
    el('dd)(user.age)
  )
```

Rendering this template will produce a DOM fragment equivalent to the following markup:

```html
<dl>
  <dt>Name</dt>
  <dd>Julien</dd>
  <dt>Age</dt>
  <dd>27</dd>
</dl>
```

See the [tests](/js-scala/forest/blob/master/forest/src/test/scala/forest/TestJSGen.scala) for more examples.

# Setup

First, setup [js-scala](http://github.com/js-scala/js-scala).

Then, you can build the forest DSL:

Go under the `forest/` subdirectory and run `sbt`. You can check the `test`s, or `publish-local`.

You may also be interested in the external DSL layer:

Go under the `compiler/` subdirectory and run `sbt publish-local`.
