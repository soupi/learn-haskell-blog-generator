# Fancy options parsing

We'd like to define a nicer interface for our program. While we could manage something
ourselves with `getArgs` and pattern matching, it is easier to get good results using a library.
We are going to use a package called
[optparse-applicative](https://hackage.haskell.org/package/optparse-applicative).

`optparse-applicative` provides us with an EDSL (yes, another one) to build
command arguments parsers. Things like commands, switches, and flags can be built
and composed together to make a parser for command-line arguments without actually
writing operations on strings as we did when we wrote our Markup parser, and will
provide other benefits such as automatic generation of usage lines, help screens,
error reporting, and more.

While `optparse-applicative`'s dependency footprint isn't very large,
it is likely that a user of our library wouldn't need command-line parsing
in this particular case, so it makes sense to add this dependency to the `executable` section
(rather than the `library` section) in the `.cabal` file:

```diff
 executable hs-blog-gen
   import: common-settings
   hs-source-dirs: app
   main-is: Main.hs
   build-depends:
       base
+    , optparse-applicative
     , hs-blog
   ghc-options:
     -O
```

## Building a command-line parser

The optparse-applicative package has pretty decent
[documentation](https://hackage.haskell.org/package/optparse-applicative-0.16.1.0#optparse-applicative),
but we will cover a few important things to pay attention to in this chapter.

In general, there are four important things we need to do:

1. Define our model - we want to define an ADT that describes the various options
   and commands for our program

2. Define a parser that will produce a value of our model type when run

3. Run the parser on our program arguments input

4. Pattern match on the model and call the right operations according to the options

### Define a model

Let's envision our command-line interface for a second, what should it
look like?

We want to be able to convert a single file or input stream to either a file
or an output stream, or we want to process a whole directory and create a new directory.
We can model it in an ADT like this:

```hs
data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath
  deriving Show

data SingleInput
  = Stdin
  | InputFile FilePath
  deriving Show

data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving Show
```

> Note that we could technically also use `Maybe FilePath` to encode both `SingleInput`
> and `SingleOutput`, but then we would have to remember what `Nothing` means
> in each context. By creating a new type with properly named constructors
> for each option we make it easier for readers of the code to understand
> the meaning of our code.

In terms of interface, we could decide that when a user would like to convert
a single input source, they would use the `convert` command, and supply the optional flags
`--input FILEPATH` and `--output FILEPATH` to read or write from a file.
When the user does not supply one or both flags, we will read or write from
the standard input/output accordingly.

If the user would like to convert a directory, they can use the `convert-dir`
command and supply the two mandatory flags `--input FILEPATH` and
`--output FILEPATH`.

### Build a parser

This is the most interesting part of the process. How do we build a parser
that fits our model?

The `optparse-applicative` library introduces a new type called `Parser`.
`Parser`, similar to `Maybe` and `IO`, has the kind `* -> *` - when it
is supplied with a saturated (or concrete) type such as `Int`, `Bool` or
`Options`, it can become a saturated type (one that has values).

 A `Parser a` represents a specification of a command-line options parser
that produces a value of type `a` when the command-line arguments are
successfully parsed.
This is similar to how `IO a` represents a description of a program
that can produce a value of type `a`. The main difference between these
two types is that while we can't convert an `IO a` to an `a`
(we just chain IO operations and have the Haskell runtime execute them),
we *can* convert a `Parser a` to a function that takes a list of strings
representing the program arguments and produces an `a` if it manages
to parse the arguments.

As we've seen with the previous EDSLs, this library uses the *combinator pattern*
as well. We need to consider the basic primitives for building
a parser, and the methods of composing small parsers into bigger
parsers.

Let's see an example for a small parser:

```hs
inp :: Parser FilePath
inp =
  strOption
    ( long "input"
      <> short 'i'
      <> metavar "FILE"
      <> help "Input file"
    )

out :: Parser FilePath
out =
  strOption
    ( long "output"
      <> short 'o'
      <> metavar "FILE"
      <> help "Output file"
    )
```

`strOption` is a parser builder. It is a function that takes a combined
*option modifiers* as an argument, and returns a parser that will parse a string.
We can specify the type to be `FilePath` because `FilePath` is an
alias to `String`. The parser builder describes how to parse the value,
and the modifiers describe its properties, such as the flag name,
the shorthand of the flag name, and how it would be described in the usage
and help messages.

> Actually `strOption` can return any string type
> that implements the interface `IsString`. There are a few such types,
> for example `Text`, a much more efficient Unicode text type from the `text` package.
> It is more efficient than `String` because while `String` is implemented as a
> linked list of `Char`, `Text` is implemented as an array of bytes.
> `Text` is usually what we should use for text values instead of `String`. We haven't
> been using it up until now because it is slightly less ergonomic to use
> than `String`. But it is often the preferred type to use for text!

As you can see, modifiers can be composed using the `<>` function,
which means modifiers implement an instance of the `Semigroup` type class!

With such an interface we don't have to supply all the modifier
options, but only the relevant ones. So if we don't want to
have a shortened flag name, we don't have to add it.

#### Functor

For the data type we've defined, having `Parser FilePath` takes us
a good step in the right direction, but it is not exactly what we need
for a `ConvertSingle`. We need a `Parser SingleInput` and a
`Parser SingleOutput`. If we had a `FilePath`, we could convert
it into `SingleInput` by using the `InputFile` constructor.
Remember, `InputFile` is also a function:

```hs
InputFile :: FilePath -> SingleInput
OutputFile :: FilePath -> SingleOutput
```

However, to convert a parser, we need functions with these types:

```hs
f :: Parser FilePath -> Parser SingleInput
g :: Parser FilePath -> Parser SingleOutput
```

Fortunately, the `Parser` interface provides us with a function to "lift"
a function like `FilePath -> SingleInput` to work on parsers, making
it a function with the type `Parser FilePath -> Parser SingleInput`.
Of course, this function will work for any input and output,
so if we have a function with the type `a -> b`, we can pass it to
that function and get a new function of the type `Parser a -> Parser b`.

This function is called `fmap`:

```hs
fmap :: (a -> b) -> Parser a -> Parser b

-- Or with its infix version
(<$>)  :: (a -> b) -> Parser a -> Parser b
```

We've seen `fmap` before in the interface of other types:

```hs
fmap :: (a -> b) -> [a] -> [b]

fmap :: (a -> b) -> IO a -> IO b
```

`fmap` is a type class function like `<>` and `show`. It belongs
to the type class [`Functor`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Functor.html#t:Functor):

```hs
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

And it has the following laws:

```hs
-- 1. Identity law:
--    if we don't change the values, nothing should change
fmap id = id

-- 2. Composition law:
--    Composing the lifted functions is the same a composing
--    them after fmap
fmap (f . g) == fmap f . fmap g
```

Any type `f` that can implement `fmap` and follow these laws can be a valid
instance of `Functor`.

> Notice how `f` has a kind `* -> *`, we can infer the kind of `f`
> by looking at the other types in the type signature of `fmap`:
>
> 1. `a` and `b` have the kind `*` because they are used as arguments/return
> types of functions
> 2. `f a` has the kind `*` because it is used as an argument to a function, therefore
> 3. `f` has the kind `* -> *`

Let's choose a data type and see if we can implement a `Functor` instance.
We need to choose a data type that has the kind `* -> *`. `Maybe` fits the bill.
We need to implement a function `fmap :: (a -> b) -> Maybe a -> Maybe b`.
Here's one very simple (and wrong) implementation:

```hs
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe func maybeX = Nothing
```

Check it yourself! It compiles successfully! But unfortunately it does not
satisfy the first law. `fmap id = id` means that
`mapMaybe id (Just x) == Just x`, however from the definition we can
clearly see that `mapMaybe id (Just x) == Nothing`.

This is a good example of how Haskell doesn't help us make sure the laws
are satisfied, and why they are important. Unlawful `Functor` instances
will behave differently from what we'd expect a `Functor` to behave.
Let's try again!

```hs
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe func maybeX =
  case maybeX of
    Nothing -> Nothing
    Just x -> Just (func x)
```

This `mapMaybe` will satisfy the functor laws. This can be proved
by doing algebra - if we can do substitution and reach the other side of the
equation in each law, then the law holds.

Functor is a very important type class, and many types implement this interface.
As we know, `IO`, `Maybe`, `[]` and `Parser` all have the kind `* -> *`,
and all allows us to map over their "payload" type.

> Often people try to look for analogies and metaphors to what a type class mean,
> but type classes with funny names like `Functor` don't usually have an
> analogy or a metaphor that fits them in all cases. It is easier to give up
> on the metaphor and think about it as it is - an interface with laws.

We can use `fmap` on `Parser` to make a parser that returns `FilePath` to
return a `SingleInput` or `SingleOutput` instead:

```hs
pInputFile :: Parser SingleInput
pInputFile = fmap InputFile parser
  where
    parser =
      strOption
        ( long "input"
          <> short 'i'
          <> metavar "FILE"
          <> help "Input file"
        )

pOutputFile :: Parser SingleOutput
pOutputFile = OutputFile <$> parser -- fmap and <$> are the same
  where
    parser =
      strOption
        ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Output file"
        )
```

#### Applicative

Now that we have two parsers,
`pInputFile :: Parser SingleInput`
and `pOutputFile :: Parser SingleOutput`,
we want to *combine* them as `Options`. Again, if we only had
`SingleInput` and `SingleOutput`, we could use the constructor `ConvertSingle`:

```hs
ConvertSingle :: SingleInput -> SingleOutput -> Options
```

Can we do a similar trick to the one we saw before with `fmap`?
Does a function exist that can lift a binary function to work
on `Parser`s instead? One with this type signature:

```
???
  :: (SingleInput -> SingleOutput -> Options)
  -> (Parser SingleInput -> Parser SingleOutput -> Parser Options)
```

Yes. This function is called `liftA2` and it is from the `Applicative`
type class. `Applicative` (also known as applicative functor) has three
primary functions:

```hs
class Functor f => Applicative f where
  pure :: a -> f a
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (<*>) :: f (a -> b) -> f a -> f b
```

[`Applicative`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Applicative.html#t:Applicative)
is another very popular type class with many instances.

Just like any `Monoid` is a `Semigroup`, any `Applicative`
is a `Functor`. This means that any type that wants to implement
the `Applicative` interface should also implement the `Functor` interface.

Beyond what a regular functor can do, which is to lift a function over
a certain `f`, applicative functors allow us to apply a function to
*multiple instances* of a certain `f`, as well as "lift" any value of type `a` into an `f a`.

You should already be familiar with `pure`, we've seen it when we
talked about `IO`. For `IO`, `pure` lets us create an `IO` action
with a specific return value without doing IO.
With `pure` for `Parser`, we can create a `Parser` that when run
will return a specific value as output without doing any parsing.

`liftA2` and `<*>` are two functions that can be implemented in
terms of one another. `<*>` is actually the more useful one between
the two. Because when combined with `fmap` (or rather the infix version `<$>`),
it can be used to apply a function with many arguments, instead of just two.

To combine our two parsers to one, we can use either `liftA2` or
a combination of `<$>` and `<*>`:

```hs
-- with liftA2
pConvertSingle :: Parser Options
pConvertSingle =
  liftA2 ConvertSingle pInputFile pOutputFile

-- with <$> and <*>
pConvertSingle :: Parser Options
pConvertSingle =
  ConvertSingle <$> pInputFile <*> pOutputFile
```

Note that both `<$>` and `<*>` associate to the left,
so we have invisible parenthesis that look like this:

```hs
pConvertSingle :: Parser Options
pConvertSingle =
  (ConvertSingle <$> pInputFile) <*> pOutputFile
```

Let's take a deeper look at the types of the sub-expressions
we have here, to prove that this type-checks:

```hs
pConvertSingle :: Parser Options

pInputFile :: Parser SingleInput
pOutputFile :: Parser SingleOutput

ConvertSingle :: SingleInput -> SingleOutput -> Options

(<$>) :: (a -> b) -> Parser a -> Parser b
  -- Specifically, here `a` is `SingleInput`
  -- and `b` is `SingleOutput -> Options`,

ConvertSingle <$> pInputFile :: Parser (SingleOutput -> Options)

(<*>) :: Parser (a -> b) -> Parser a -> Parser b
  -- Specifically, here `a -> b` is `SingleOutput -> Options`
  -- so `a` is `SingleOutput` and `b` is `Options`

-- So we get:
(ConvertSingle <$> pInputFile) <*> pOutputFile :: Parser Options
```

With `<$>` and `<*>` we can chain as many parsers (or any applicative really)
as we want. This is because of two things: currying and parametric polymorphism.
Because functions in Haskell take exactly one argument and return exactly one,
any multiple argument function can be represented as `a -> b`.

> You can find the laws for the applicative functors in this article called
> [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Laws_2), which
> talks about various useful type classes and their laws.

Applicative functor is a very important concept and will appear in various
parser interfaces (not just for command-line arguments, but also JSON
parsers and general parsers), I/O, concurrency, non-determinism, and more.
The reason this library is called optparse-applicative is because
it uses the `Applicative` interface as the main API for
constructing parsers.

---

**Exercise**: create a similar interface for the `ConvertDir` constructor of `Options`.

<details><summary>Solution</summary>

```hs
pInputDir :: Parser FilePath
pInputDir =
  strOption
    ( long "input"
      <> short 'i'
      <> metavar "DIRECTORY"
      <> help "Input directory"
    )

pOutputDir :: Parser FilePath
pOutputDir =
  strOption
    ( long "output"
      <> short 'o'
      <> metavar "DIRECTORY"
      <> help "Output directory"
    )

pConvertDir :: Parser Options
pConvertDir =
  ConvertDir <$> pInputDir <*> pOutputDir
```

</details>

---

#### Alternative

One thing we forgot about is that each input and output for
`ConvertSingle` could also potentially use the standard input and output instead.
Up until now we only offered one option: reading from or writing to a file
by specifying the flags `--input` and `--output`.
However, we'd like to make these flags optional, and when they are
not specified, use the alternative standard i/o. We can do that by using
the function `optional` from `Control.Applicative`:

```hs
optional :: Alternative f => f a -> f (Maybe a)
```

`optional` works on types which implement instances of the
[`Alternative`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Applicative.html#t:Alternative) type class:

```hs
class Applicative f => Alternative f where
  (<|>) :: f a -> f a -> f a
  empty :: f a
```

`Alternative` looks very similar to the `Monoid` type class,
but it works on applicative functors. This type class isn't
very common and is mostly used for parsing libraries as far as I know.
It provides us with an interface to combine two `Parser`s -
if the first one fails to parse, try the other.
It also provides other useful functions such as `optional`,
which will help us with our case:

```hs
pSingleInput :: Parser SingleInput
pSingleInput =
  fromMaybe Stdin <$> optional pInputFile

pSingleOutput :: Parser SingleOutput
pSingleOutput =
  fromMaybe Stdout <$> optional pOutputFile
```

Note that with `fromMaybe :: a -> Maybe a -> a` we can extract
the `a` out of the `Maybe` by supplying a value for the `Nothing` case.

Now we can use these more appropriate functions in `pConvertSingle` instead:

```hs
pConvertSingle :: Parser Options
pConvertSingle =
  ConvertSingle <$> pSingleInput <*> pSingleOutput
```

#### Commands and subparsers

We currently have two possible operations in our interface,
convert a single source, or convert a directory. A nice interface for
selecting the right operation would be via commands.
If the user would like to convert a single source, they can use
`convert`, for a directory, `convert-dir`.

We can create a parser with commands with the `subparser` and `command`
functions:

```hs
subparser :: Mod CommandFields a -> Parser a

command :: String -> ParserInfo a -> Mod CommandFields a
```

`subparser` takes *command modifiers* (which can be constructed
with the `command` function) as input, and produces a `Parser`.
`command` takes the command name (in our case "convert" or "convert-dir")
and a `ParserInfo a`, and produces a command modifier. As we've seen
before these modifiers have a `Monoid` instance and they can be
composed, meaning that we can append multiple commands to serve as alternatives.

A `ParserInfo a` can be constructed with the `info` function:

```hs
info :: Parser a -> InfoMod a -> ParserInfo a
```

This function wraps a `Parser` with some additional information
such as a helper message, description, and more, so that the program
itself and each sub command can print some additional information.

Let's see how to construct a `ParserInfo`:

```hs
pConvertSingleInfo :: ParserInfo Options
pConvertSingleInfo =
  info
    (helper <*> pConvertSingle)
    (progDesc "Convert a single markup source to html")
```
Note that `helper` adds a helper output screen in case the parser fails.

Let's also build a command:

```hs
pConvertSingleCommand :: Mod CommandFields Options
pConvertSingleCommand =
  command "convert" pConvertSingleInfo
```

Try creating a `Parser Options` combining the two options with `subparser`.

<details><summary>Solution</summary>

```hs
pOptions :: Parser Options
pOptions =
  subparser
    ( command
      "convert"
      ( info
        (helper <*> pConvertSingle)
        (progDesc "Convert a single markup source to html")
      )
      <> command
      "convert-dir"
      ( info
        (helper <*> pConvertDir)
        (progDesc "Convert a directory of markup files to html")
      )
    )
```

</details>

#### ParserInfo

Since we finished building a parser, we should wrap it up in a `ParserInfo`
and add some information to it to make it ready to run:

```hs
opts :: ParserInfo Options
opts =
  info (helper <*> pOptions)
    ( fullDesc
      <> header "hs-blog-gen - a static blog generator"
      <> progDesc "Convert markup files or directories to html"
    )
```

### Running a parser

`optparse-applicative` provides a non-`IO` interface to parse arguments,
but the most convenient way to use it is to let it take care of fetching
program arguments, try to parse them, and throw errors and help messages in case
it fails. This can be done with the function `execParser :: ParserInfo a -> IO a`.

We can place all this options parsing stuff in a new module
and then import it from `app/Main.hs`. Let's do that.
Here's what we have up until now:

<details><summary>app/OptParse.hs</summary>

```hs
-- | Command-line options parsing

module OptParse
  ( Options(..)
  , SingleInput(..)
  , SingleOutput(..)
  , parse
  )
  where

import Data.Maybe (fromMaybe)
import Options.Applicative

------------------------------------------------
-- * Our command-line options model

-- | Model
data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath
  deriving Show

-- | A single input source
data SingleInput
  = Stdin
  | InputFile FilePath
  deriving Show

-- | A single output sink
data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving Show

------------------------------------------------
-- * Parser

-- | Parse command-line options
parse :: IO Options
parse = execParser opts

opts :: ParserInfo Options
opts =
  info (pOptions <**> helper)
    ( fullDesc
      <> header "hs-blog-gen - a static blog generator"
      <> progDesc "Convert markup files or directories to html"
    )

-- | Parser for all options
pOptions :: Parser Options
pOptions =
  subparser
    ( command
      "convert"
      ( info
        (helper <*> pConvertSingle)
        (progDesc "Convert a single markup source to html")
      )
      <> command
      "convert-dir"
      ( info
        (helper <*> pConvertDir)
        (progDesc "Convert a directory of markup files to html")
      )
    )

------------------------------------------------
-- * Single source to sink conversion parser

-- | Parser for single source to sink option
pConvertSingle :: Parser Options
pConvertSingle =
  ConvertSingle <$> pSingleInput <*> pSingleOutput

-- | Parser for single input source
pSingleInput :: Parser SingleInput
pSingleInput =
  fromMaybe Stdin <$> optional pInputFile

-- | Parser for single output sink
pSingleOutput :: Parser SingleOutput
pSingleOutput =
  fromMaybe Stdout <$> optional pOutputFile

-- | Input file parser
pInputFile :: Parser SingleInput
pInputFile = fmap InputFile parser
  where
    parser =
      strOption
        ( long "input"
          <> short 'i'
          <> metavar "FILE"
          <> help "Input file"
        )

-- | Output file parser
pOutputFile :: Parser SingleOutput
pOutputFile = OutputFile <$> parser
  where
    parser =
      strOption
        ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Output file"
        )

------------------------------------------------
-- * Directory conversion parser

pConvertDir :: Parser Options
pConvertDir =
  ConvertDir <$> pInputDir <*> pOutputDir

-- | Parser for input directory
pInputDir :: Parser FilePath
pInputDir =
  strOption
    ( long "input"
      <> short 'i'
      <> metavar "DIRECTORY"
      <> help "Input directory"
    )

-- | Parser for output directory
pOutputDir :: Parser FilePath
pOutputDir =
  strOption
    ( long "output"
      <> short 'o'
      <> metavar "DIRECTORY"
      <> help "Output directory"
    )
```

</details>

### Pattern matching on Options

After running the command-line arguments parser, we can pattern match
on our model and call the right functions. Currently, our program
does not expose this kind of API. So let's go to our `src/HsBlog.hs`
module and change the API. We can delete `main` from that file and
add two new functions instead:

```hs
convertSingle :: Html.Title -> Handle -> Handle -> IO ()

convertDirectory :: FilePath -> FilePath -> IO ()
```

[`Handle`](https://hackage.haskell.org/package/base-4.16.4.0/docs/System-IO.html#t:Handle)
is an I/O abstraction over file system objects, including `stdin` and `stdout`.
Before, we used `writeFile` and `getContents` - these functions either
get a `FilePath` to open and work on, or they assume the `Handle` is the standard I/O.
We can use the explicit versions that take a `Handle` from `System.IO` instead:

```hs
convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)
```

We will leave `convertDirectory` unimplemented for now and implement it in the next chapter.

In `app/Main.hs`, we will need to pattern match on the `Options` and
prepare to call the right functions from `HsBlog`.

Let's look at our full `app/Main.hs` and `src/HsBlog.hs`:

<details><summary>app/Main.hs</summary>

```hs
-- | Entry point for the hs-blog-gen program

module Main where

import OptParse
import qualified HsBlog

import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.IO

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output ->
      HsBlog.convertDirectory input output

    ConvertSingle input output -> do
      (title, inputHandle) <-
        case input of
          Stdin ->
            pure ("", stdin)
          InputFile file ->
            (,) file <$> openFile file ReadMode

      outputHandle <-
        case output of
          Stdout -> pure stdout
          OutputFile file -> do
            exists <- doesFileExist file
            shouldOpenFile <-
              if exists
                then confirm
                else pure True
            if shouldOpenFile
              then
                openFile file WriteMode
              else
                exitFailure

      HsBlog.convertSingle title inputHandle outputHandle
      hClose inputHandle
      hClose outputHandle

------------------------------------------------
-- * Utilities

-- | Confirm user action
confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/n)" *>
    getLine >>= \answer ->
      case answer of
        "y" -> pure True
        "n" -> pure False
        _ -> putStrLn "Invalid response. use y or n" *>
          confirm
```

</details>

<details><summary>src/HsBlog.hs</summary>

```hs
-- HsBlog.hs
module HsBlog
  ( convertSingle
  , convertDirectory
  , process
  )
  where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html
import HsBlog.Convert (convert)

import System.IO

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = error "Not implemented"

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse
```

</details>

We need to make a few small changes to the `.cabal` file.

First, we need to add the dependency `directory` to the `executable`,
because we use the library `System.Directory` in `Main`.

Second, we need to list `OptParse` in the list of modules in
the `executable`.

```diff
 executable hs-blog-gen
   import: common-settings
   hs-source-dirs: app
   main-is: Main.hs
+  other-modules:
+    OptParse
   build-depends:
       base
+    , directory
     , optparse-applicative
     , hs-blog
   ghc-options:
     -O
```

## Summary

We've learned about a new fancy library called `optparse-applicative`
and used it to create a fancier command-line interface in a declarative way.
See the result of running `hs-blog-gen --help` (or the equivalent
`cabal`/`stack` commands we discussed in the last chapter):

```
hs-blog-gen - a static blog generator

Usage: hs-blog-gen COMMAND
  Convert markup files or directories to html

Available options:
  -h,--help                Show this help text

Available commands:
  convert                  Convert a single markup source to html
  convert-dir              Convert a directory of markup files to html
```

Along the way we've learned two powerful new abstractions, `Functor`
and `Applicative`, as well as revisited an abstraction
called `Monoid`. With this library we've seen another example
of the usefulness of these abstractions for constructing APIs and EDSLs.

We will continue to meet these abstractions in the rest of the book.

---

**Bonus exercise**: Add another flag named `--replace` to indicate that
if the output file or directory already exists, it's okay to replace them.

---

> You can view the git commit of
> [the changes we've made](https://github.com/soupi/learn-haskell-blog-generator/commit/d0d76aad632fe3abd8701e44db5ba687e0c7ac96)
> and the [code up until now](https://github.com/soupi/learn-haskell-blog-generator/tree/d0d76aad632fe3abd8701e44db5ba687e0c7ac96).
