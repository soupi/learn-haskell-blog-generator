# Exceptions

The [Control.Exception](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html)
module provides us with the ability to
[throw](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#v:throwIO)
exceptions from `IO` code,
[`catch`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#g:5)
Haskell exceptions in `IO` code, and even convert them to `IO (Either ...)`
with the function [`try`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#g:7):

```hs
throwIO :: Exception e => e -> IO a

catch
  :: Exception e
  => IO a         -- The computation to run
  -> (e -> IO a)  -- Handler to invoke if an exception is raised
  -> IO a

try :: Exception e => IO a -> IO (Either e a)
```

The important part of these type signatures is the
[`Exception`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#t:Exception)
type class. By making a type an instance of the `Exception` type class, we can throw it
and catch it in `IO` code:

```hs
{-# language LambdaCase #-}

import Control.Exception
import System.IO

data MyException
  = ErrZero
  | ErrOdd Int
  deriving Show

instance Exception MyException

sayDiv2 :: Int -> IO ()
sayDiv2 n
  | n == 0 = throwIO ErrZero
  | n `mod` 2 /= 0 = throwIO (ErrOdd n)
  | otherwise = print (n `div` 2)

main :: IO ()
main =
  catch
    ( do
      putStrLn "Going to print a number now."
      sayDiv2 7
      putStrLn "Did you like it?"
    )
    ( \case
      ErrZero ->
        hPutStrLn stderr "Error: we don't support dividing zeroes for some reason"
      ErrOdd n ->
        hPutStrLn stderr ("Error: " <> show n <> " is odd and cannot be divided by 2")
    )
```

> Note: we are using two new things here: guards, and the `LambdaCase` language extension.
>
> 1. Guards as seen in `sayDiv2` are just a nicer syntax around `if-then-else` expressions.
>    Using guards we can have multiple `if` branches and finally use the `else` branch
>    by using `otherwise`. After each guard (`|`) there's a condition, after the condition there's
>    a `=` and then the expression (the part after `then` in an `if` expression)
>
> 2. LambdaCase as seen in `catch`, is just a syntactic sugar to save a few characters,
>    instead of writing `\e -> case e of`, we can write `\case`. It requires enabling the
>    `LambdaCase` extension
>
>    #### Language extensions
>
>    Haskell is a standardized language. However, GHC provides *extensions* to the language -
>    additional features that aren't covered in the 98 or 2010 standards of Haskell.
>    Features such as syntactic extensions (like LambdaCase above), extensions to the type checker,
>    and more.
>
>    These extensions can be added by adding `{-# language <extension-name> #-}`
>    (the `language` part is case insensitive)
>    to the top of a Haskell source file, or they can be set globally for an entire project by
>    specifying them in the
>    [default-extensions](https://cabal.readthedocs.io/en/stable/cabal-package.html#pkg-field-default-extensions)
>    section in the `.cabal file`.
>
>    The list of language extensions can be found in the
>    [GHC manual](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts.html),
>    feel free to browse it, but don't worry about trying to memorize all the extensions.


This example, of course, is an example that would work much better using `Either` and separating
the division and printing à la 'functional core, imperative shell'. But as an example it works.
We have created a custom exception and handled it specifically outside an `IO` block.
However, we have not handled exceptions that might be raised by `putStrLn`.
What if, for example, for some reason we close the `stdout` handle before this block:

```hs
main :: IO ()
main = do
  hClose stdout
  catch
    ( do
      putStrLn "Going to print a number now."
      sayDiv2 7
      putStrLn "Did you like it?"
    )
    ( \case
      ErrZero ->
        hPutStrLn stderr "Error: we don't support dividing zeroes for some reason"
      ErrOdd n ->
        hPutStrLn stderr ("Error: " <> show n <> " is odd and cannot be divided by 2")
    )
```

Our program will crash with an error:

```
ghc: <stdout>: hFlush: illegal operation (handle is closed)
```

First, how do we know which exception we should handle? Some functions' documentation
include this, but unfortunately `putStrLn`'s does not. We could guess from the
[list of instances](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#i:Exception)
the `Exception` type class has; I think
[`IOException`](https://hackage.haskell.org/package/base-4.16.4.0/docs/GHC-IO-Exception.html#t:IOException) fits. Now, how can we handle this case as well? We can chain catches:

```hs
-- need to add these at the top

{-# language ScopedTypeVariables #-}

import GHC.IO.Exception (IOException(..))

main :: IO ()
main = do
  hClose stdout
  catch
    ( catch
      ( do
        putStrLn "Going to print a number now."
        sayDiv2 7
        putStrLn "Did you like it?"
      )
      ( \case
        ErrZero ->
          hPutStrLn stderr "Error: we don't support dividing zeroes for some reason"
        ErrOdd n ->
          hPutStrLn stderr ("Error: " <> show n <> " is odd and cannot be divided by 2")
      )
    )
    ( \(e :: IOException) ->
      -- we can check if the error was an illegal operation on the stderr handle
      if ioe_handle e /= Just stderr && ioe_type e /= IllegalOperation
        then pure () -- we can't write to stderr because it is closed
        else hPutStrLn stderr (displayException e)
    )
```

> We use the `ScopedTypeVariables` to be able to specify types inside let expressions,
> lambdas, pattern matching and more.

Or we could use the convenient function
[`catches`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#v:catches)
to pass a list of exception
[handlers](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#t:Handler):

```hs
main :: IO ()
main = do
  hClose stdout
  catches
    ( do
      putStrLn "Going to print a number now."
      sayDiv2 7
      putStrLn "Did you like it?"
    )
    [ Handler $ \case
      ErrZero ->
        hPutStrLn stderr "Error: we don't support dividing zeroes for some reason"
      ErrOdd n ->
        hPutStrLn stderr ("Error: " <> show n <> " is odd and cannot be divided by 2")

    , Handler $ \(e :: IOException) ->
      -- we can check if the error was an illegal operation on the stderr handle
      if ioe_handle e /= Just stderr && ioe_type e /= IllegalOperation
        then pure () -- we can't write to stderr because it is closed
        else hPutStrLn stderr (displayException e)
    ]
```

> As an aside, `Handler` uses a concept called
> [existentially quantified types](https://en.m.wikibooks.org/wiki/Haskell/Existentially_quantified_types)
> to hide inside it a function that takes an arbitrary type that implements `Exception`.
> This is why we can encode a seemingly heterogeneous list of functions that handle exceptions
> for `catches` to take as input.
> This pattern is rarely useful, but I've included it here to avoid confusion.

And if we wanted to catch any exception, we'd catch `SomeException`:

```hs
main :: IO ()
main = do
  hClose stdout
  catch
    ( do
      putStrLn "Going to print a number now."
      sayDiv2 7
      putStrLn "Did you like it?"
    )
    ( \(SomeException e) ->
      hPutStrLn stderr (show e)
    )
```

This could also go in `catches` as the last element in the list if we wanted specialized
handling for other scenarios.

A couple more functions worth knowing are
[`bracket`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#v:bracket)
and [`finally`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#v:finally).
These functions can help us handle resource acquisition more safely when errors are present.

---

In our `main` in the `app/Main.hs` file, we do a small ritual opening and closing handles.
Are there scenarios where we would clean-up after ourselves (meaning, close handles we've
opened)? Which parts of the code could throw an exception? Which handles won't get closed?

- Try to use `bracket` to make sure we always close a handle afterwards, even if an exception
  is thrown, and avoid closing the handle for the `stdin` and `stdout` cases
  <details><summary>Hint</summary>We might need to use continuation-passing style,
  passing a function that takes a parameter to a function that produces a parameter
  and calls it with that parameter.
  </details>
- How can we avoid duplicating the `outputHandle` code, for the `Stdin` and `InputFile`
  branches? <details><summary>Hint</summary> Use `let`.</details>

<details><summary>Answer</summary>

```hs
import Control.Exception (bracket)

main :: IO ()
main = do
...

    ConvertSingle input output ->
      let
        -- Here, action is the next steps we want to do.
        -- It takes as input the values we produce,
        -- uses it, and then returns control for us to clean-up
        -- afterwards.
        withInputHandle :: (String -> Handle -> IO a) -> IO a
        withInputHandle action =
          case input of
            Stdin ->
              action "" stdin
            InputFile file ->
              bracket
                (openFile file ReadMode)
                hClose
                (action file)

        -- Note that in both functions our action can return any `a`
        -- it wants.
        withOutputHandle :: (Handle -> IO a) -> IO a
        withOutputHandle action =
          case output of
            Stdout ->
              action stdout
            OutputFile file -> do
              exists <- doesFileExist file
              shouldOpenFile <-
                if exists
                  then confirm
                  else pure True
              if shouldOpenFile
                then
                  bracket (openFile file WriteMode) hClose action
                else
                  exitFailure
      in
        withInputHandle (\title -> withOutputHandle . HsBlog.convertSingle title)
```

</details>

There's action a custom function that does similar thing to
`bracket (openFile file <mode>) hClose`, it's called
[withFile](https://hackage.haskell.org/package/base-4.17.0.0/docs/System-IO.html#v:withFile).
Keep an eye out for functions that start with the prefix `with`, they are probably using the
same pattern of continuation-passing style.

---

## Summary

Exceptions are useful and often necessary when we work with `IO` and want to make sure
our program is handling errors gracefully. They have an advantage over `Either` in that
we can easily compose functions that may throw errors of different types, but also have
a disadvantage of not encoding types as return values, and therefore does not force us
to handle them.

For Haskell, the language designers have made a choice for us by designing `IO` to
use exceptions instead of `Either`. And this is what I would recommend for
handling your own effectful computations. However, I think that `Either` is more
appropriate for uneffectful code, because it forces us to acknowledge and handle errors
(eventually) thus making our programs more robust. And also because we can only
catch exceptions in `IO` code.
