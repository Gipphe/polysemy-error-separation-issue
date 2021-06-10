# polysemy-error-separation-issue

For a given function

```haskell
f :: Member Foo r => a -> Sem r b
```

and a given effect

```haskell
data Foo m a where
  Foo :: Foo m a
```

it is sensible to assume that `f` can use `Foo`, with no explicitly visible
exceptions or `Error`s, regardless what the particular interpreter used for
`Foo` throws or does.

This, however, seems to not be the case. Given a function

```haskell
g :: Members '[Foo, Error e] r => a -> Sem r b
```

and an interpreter for `Foo` which throws `e`s

```haskell
runFoo :: Member (Error e) r => Sem (Foo ': r) a -> Sem r a
runFoo = interpret $ \Foo -> throw e
```

`g` is able to `catch` the error thrown by the `runFoo` interpreter. In many
ways, this is convenient, because we are able to react to lower-level `Error`s
that occur in interpreters in business-level code, but it is nonetheless
confusing and unintuitive.
