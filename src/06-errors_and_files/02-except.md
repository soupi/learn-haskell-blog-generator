# Either with IO?

When we create `IO` actions that may require I/O we risk running into all kinds of errors.
For example, when we use `writeFile`, we could run out of disk space in the middle of writing,
or the file might be write protected. While these scenarios aren't super common, they are definitely
possible.

We could've potentially encoded Haskell functions like `readFile` and `writeFile` as `IO` operations
that return `Either`, for example:

```hs
readFile :: FilePath -> IO (Either ReadFileError String)
writeFile :: FilePath -> String -> IO (Either WriteFileError ())
```

However there are a couple of issues here, the first is that composing `IO` actions
becomes more difficult. Previously we could write:

```hs
readFile "input.txt" >>= writeFile "output.html"
```

But now the types no longer match - `readFile` will return an `Either ReadFileError String` when executed,
but `writeFile` wants to take a `String` as input. We are forced to handle the error
before calling `writeFile`.

## Composing IO + Either using ExceptT

One way to handle this is by using **monad transformers**. Monad transformers provide a way
to stack monad capabilities on top of one another. They are called transformers because
**they take a type that has an instance of monad as input, and return a new type that
implements the monad interface, stacking a new capability on top of it**.

For example, if we want to compose values of a type that is equivalent to `IO (Either Error a)`,
using the monadic interface (the function `>>=`), we can use a monad transformer
called [`ExceptT`](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Except.html#g:2)
and stack it over `IO`.
Let's see how `ExceptT` is defined:

```hs
newtype ExceptT e m a = ExceptT (m (Either e a))
```

Remember, a `newtype` is a new name for an existing type. And if we substitute
`e` with `Error` and `m` with `IO` we get exactly `IO (Either Error a)` as we wanted.
And we can convert an `ExceptT Error IO a` into `IO (Either Error a)` using
the function `runExceptT`:

```hs
runExceptT :: ExceptT e m a -> m (Either e a)
```

`ExceptT` implements the monadic interface in a way that combines the capabilities of
`Either`, and whatever `m` it takes. Because `ExceptT e m` has a `Monad` instance,
a specialized version of `>>=` would look like this:

```hs
-- Generalized version
(>>=) :: Monad m => m a -> (a -> m b) -> m b

-- Specialized version, replace the `m` above with `ExceptT e m`.
(>>=) :: Monad m => ExceptT e m a -> (a -> ExceptT e m b) -> ExceptT e m b
```

Note that the `m` in the specialized version still needs to be an instance of `Monad`.

---

Unsure how this works? Try to implement `>>=` for `IO (Either Error a)`:

```hs
bindExceptT :: IO (Either Error a) -> (a -> IO (Either Error b)) -> IO (Either Error b)
```

<details><summary>Solution</summary>

```hs
bindExceptT :: IO (Either Error a) -> (a -> IO (Either Error b)) -> IO (Either Error b)
bindExceptT mx f = do
  x <- mx -- `x` has the type `Either Error a`
  case x of
    Left err -> pure (Left err)
    Right y -> f y
```

Note that we didn't actually use the implementation details of `Error` or `IO`,
`Error` isn't mentioned at all, and for `IO` we only used the monadic interface with
the do notation. We could write the same function with a more generalized type signature:

```hs
bindExceptT :: Monad m => m (Either e a) -> (a -> m (Either e b)) -> m (Either e b)
bindExceptT mx f = do
  x <- mx -- `x` has the type `Either e a`
  case x of
    Left err -> pure (Left err)
    Right y -> f y
```

And because `newtype ExceptT e m a = ExceptT (m (Either e a))` we can just
pack and unpack that `ExceptT` constructor and get:


```hs
bindExceptT :: Monad m => ExceptT e m a -> (a -> ExceptT e m b) -> ExceptT e m b
bindExceptT mx f = ExceptT $ do
  -- `runExceptT mx` has the type `m (Either e a)`
  -- `x` has the type `Either e a`
  x <- runExceptT mx
  case x of
    Left err -> pure (Left err)
    Right y -> runExceptT (f y)
```

</details>

---

> Note that when stacking monad transformers, the order in which we stack them matters.
> With `ExceptT Error IO a`, we have an `IO` operation that when run will return `Either`
> an error or a value.

`ExceptT` can enjoy both worlds - we can return error values using the function `throwError`:

```hs
throwError :: e -> ExceptT e m a
```

and we can "lift" functions that return a value of the underlying monadic type `m` to return
a value of `ExceptT e m a` instead:

```hs
lift :: m a -> ExceptT e m a
```

for example:

```hs
getLine :: IO String

lift getLine :: ExceptT e IO String
```

> Actually, `lift` is also a type class function from `MonadTrans`, the type class
> of monad transformers. So technically `lift getLine :: MonadTrans t => t IO String`,
> but we are specializing for concreteness.


Now, if we had:

```hs
readFile :: FilePath -> ExceptT IOError IO String

writeFile :: FilePath -> String -> ExceptT IOError IO ()
```

We could compose them again without issue:

```hs
readFile "input.txt" >>= writeFile "ouptut.html"
```

But remember - the error type `e` (in both the case `Either` and `Except`)
must be the same between composed functions! This means that the type representing
errors for both `readFile` and `writeFile` must be the same - that would also
force anyone using these functions to handle these errors - should a user who
called `writeFile` be required to handle a "file not found" error? Should a user
who called `readFile` be required to handle an "out of disk space" error?
There are many many more possible IO errors! "network unreachable", "out of memory",
"cancelled thread", we cannot require a user to handle all these errors, or
even cover them all in a data type.

So what do we do?

We give up on this approach **for IO code**, and use a different one: Exceptions,
as we'll see in the next chapter.

> Note - when we stack `ExceptT` on top of a different type called
> [`Identity`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Functor-Identity.html)
> that also implements the `Monad` interface, we get a type that is exactly like `Either`
> called [`Except`](https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-Except.html#t:Except)
> (without the `T` at the end). You might sometimes want to use `Except` instead of `Either`
> because it has a more appropriate name and better API for error handling than `Either`.
