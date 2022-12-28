# Lets code already!

This was a long info dump. Let's practice what we've learned. We want to:

- Create the output directory
- Grab all file names in a directory
- Filter them according to their extension
- Process .txt files
- Copy other files without modification
- Parse each text file, build an index of the result,
  convert the files to HTML, and write everything to the target directory


> Note: I did not write this code immediately in the final form it was presented.
> It was an iterative process of writing code, refactoring, splitting functions, changing
> type signatures, and more. When solving a coding problem, start small and simple,
> do the thing that works, and refactor it when it makes sense and makes the code clearer
> and more modular. In Haskell we pride ourselves in our ability to refactor code and improve
> it over time, and that principle holds when writing new software as well!

## New module

Let's create a new module, `HsBlog.Directory`, which will be responsible for handling
directories and multiple files. From this module we will export the `convertDirectory`
and `buildIndex` functions we've defined before:

```hs
-- | Process multiple files and convert directories

module HsBlog.Directory
  ( convertDirectory
  , buildIndex
  )
  where
```

In this module we are going to use the
[directory](https://hackage.haskell.org/package/directory-1.3.7.0/docs/System-Directory.html)
and [filepath](https://hackage.haskell.org/package/filepath-1.4.2.1/docs/System-FilePath.html)
libraries to manipulate directories, files and filepaths.
We'll use the new abstractions we've learned, `Traversable` and `Monad`, and the concepts
and types we've learned about: `Either`, `IO` and exceptions.

For all of that, we need quite a few imports:

```hs
import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html
import HsBlog.Convert (convert, convertStructure)

import Data.List (partition)
import Data.Traversable (for)
import Control.Monad (void, when)

import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, displayException, SomeException(..))
import System.Exit (exitFailure)
import System.FilePath
  ( takeExtension
  , takeBaseName
  , (<.>)
  , (</>)
  , takeFileName
  )
import System.Directory
  ( createDirectory
  , removeDirectoryRecursive
  , listDirectory
  , doesDirectoryExist
  , copyFile
  )
```

If you are unsure what a specific function we're using does, look it up at
[Hoogle](https://hoogle.haskell.org/),
read the type signature and the documentation, and play around with it in `ghci`.

## Converting a directory

We can start by describing the high-level function `convertDirectory` which
encapsulates many smaller functions, each responsible for doing a specific thing.
`convertDirectory` is quite imperative looking, and looks like a different way to
describe the steps of completing our task:

```hs
-- | Copy files from one directory to another, converting '.txt' files to
--   '.html' files in the process. Recording unsuccessful reads and writes to stderr.
--
-- May throw an exception on output directory creation.
convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory inputDir outputDir = do
  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
  createOutputDirectoryOrExit outputDir
  let
    outputHtmls = txtsToRenderedHtml filesToProcess
  copyFiles outputDir filesToCopy
  writeFiles outputDir outputHtmls
  putStrLn "Done."
```

Here we trust that each `IO` function handles errors responsibly,
and terminates the project when necessary.

Let's examine the steps in order.

### `getDirFilesAndContent`

```hs
-- | The relevant directory content for our application
data DirContents
  = DirContents
    { dcFilesToProcess :: [(FilePath, String)]
      -- ^ File paths and their content
    , dcFilesToCopy :: [FilePath]
      -- ^ Other file paths, to be copied directly
    }

-- | Returns the directory content
getDirFilesAndContent :: FilePath -> IO DirContents

```

`getDirFilesAndContent` is responsible for providing the relevant files for processing --
both the ones we need to convert to markup (and their textual content) and other files we
might want to copy as-is (such as images and style-sheets):

```hs
-- | Returns the directory content
getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let
    (txtFiles, otherFiles) =
      partition ((== ".txt") . takeExtension) files
  txtFilesAndContent <-
    applyIoOnList readFile txtFiles >>= filterAndReportFailures
  pure $ DirContents
    { dcFilesToProcess = txtFilesAndContent
    , dcFilesToCopy = otherFiles
    }
```

This function does 4 important things:

1. Lists all the files in the directory
2. Splits the files into 2 groups according to their file extension
3. Reads the contents of the .txt files and report when files fail to be read
4. Returns the results. We've defined a data type to make the result content more obvious

Part (3) is a little bit more involved than the rest, let's explore it.

#### `applyIoOnList`

---

`applyIoOnList` has the following type signature:

```hs
applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
```

It tries to apply an `IO` function on a list of values, and document successes and failures.

Try to implement it! If you need a hint for which functions to use, see the import list
we wrote earlier.

<details><summary>Answer</summary>

```hs
-- | Try to apply an IO function on a list of values, document successes and failures
applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList action inputs = do
  for inputs $ \input -> do
    maybeResult <-
      catch
        (Right <$> action input)
        ( \(SomeException e) -> do
          pure $ Left (displayException e)
        )
    pure (input, maybeResult)
```

</details>

---

`applyIoOnList` is a higher order function that applies a particular `IO` function
(in our case `readFile`) on a list of things (in our case `FilePath`s).
For each thing, it returns the thing itself along with the result of
applying the `IO` function as an `Either`, where the `Left` side is a `String`
representation of an error if one occurred.

Notice how much the type of this function tells us about what it might do.
Because the types are polymorphic, there is nothing else to do with
the `a`s other than apply them to the function, and nowhere to generate `b`
from other than the result of the function.

> Note: when I first wrote this function, it was specialized to work only on `readFile`,
> take specifically `[FilePath]` and return `IO [(FilePath, Either String String)]`.
> But after running into other use cases where I could use it (`writeFiles` and `copyFiles`)
> I refactored out the `action`, the input type and the return type.

This function uses exceptions to catch any error that might be thrown, and encodes
both the failure and success cases in the type system using `Either`, delaying
the handling of exceptions to the function caller while making sure it won't
be forgotten!

Next, let's look at the function that handles the errors by reporting and then filtering out
all the cases that failed.

#### `filterAndReportFailures`

---

`filterAndReportFailures` has the following type signature:

```hs
filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
```

It filters out unsuccessful operations on files and reports errors to the stderr.

Try to implement it!

<details><summary>Answer</summary>

```hs
-- | Filter out unsuccessful operations on files and report errors to stderr.
filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap $ \(file, contentOrErr) ->
    case contentOrErr of
      Left err -> do
        hPutStrLn stderr err
        pure []
      Right content ->
        pure [(file, content)]
```

This code may seem a bit surprising - how come we can use `foldMap` here? Reminder,
the type of `foldMap` is:

```hs
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
```

If we specialize this function for our use case, substituting the general type
with the types we are using, we learn that `IO [(a, b)]` is a monoid.
And indeed - `[a]` is a monoid for any `a` with `[]` (the empty list) as `mempty`
and `++` as `<>`, but also `IO a` is a monoid for any `a` that is itself
a monoid with `pure mempty` as `mempty` and `liftA2 (<>)` as `<>`!

Using these instances, we can `map` over the content, handle errors, and return
an empty list to filter out a failed case, or a singleton list to keep the result.
And the `fold` in `foldMap` will concatenate the resulting list where we return
all of the successful cases!

If you've written this in a different way that does the same thing, that's fine too!
It's just nice to see how sometimes abstractions can be used to write concise code.

</details>

---

These functions are responsible for fetching the right information. Next,
let's look at the code for creating a new directory.

### `createOutputDirectoryOrExit`

```hs
-- | Creates an output directory or terminates the program
createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit outputDir =
  whenIO
    (not <$> createOutputDirectory outputDir)
    (hPutStrLn stderr "Cancelled." *> exitFailure)

-- | Creates the output directory.
--   Returns whether the directory was created or not.
createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory dir = do
  dirExists <- doesDirectoryExist dir
  create <-
    if dirExists
      then do
        override <- confirm "Output directory exists. Override?"
        when override (removeDirectoryRecursive dir)
        pure override
      else
        pure True
  when create (createDirectory dir)
  pure create
```

`createOutputDirectoryOrExit` itself is not terribly exciting, it does
what it is named -- it tries to create the output directory, and exits the
program in case it didn't succeed.

`createOutputDirectory` is the function that actually does the heavy lifting.
It checks if the directory already exists, and checks if the user would like to
override it. If they do, we remove it and create the new directory; if they don't,
we do nothing and report their decision.

### `txtsToRenderedHtml`

```hs
let
  outputHtmls = txtsToRenderedHtml filesToProcess
```

---

In this part of the code we convert files to markup and change the
input file paths to their respective output file paths (`.txt` -> `.html`).
We then build the index page, and convert everything to HTML.

Implement `txtsToRenderedHtml`, which has the following type signature:

```hs
txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
```

<details><summary>Hint</summary>

I implemented this by defining three functions:

```hs
txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]

toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)

convertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)
```

</details>

.

<details><summary>Answer</summary>

```hs
-- | Convert text files to Markup, build an index, and render as html.
txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml txtFiles =
  let
    txtOutputFiles = map toOutputMarkupFile txtFiles
    index = ("index.html", buildIndex txtOutputFiles)
  in
    map (fmap Html.render) (index : map convertFile txtOutputFiles)

toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)
toOutputMarkupFile (file, content) =
  (takeBaseName file <.> "html", Markup.parse content)

convertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile (file, doc) = (file, convert file doc)
```

One possibly surprising thing about this code could be the `map (fmap Html.render)`
part. We can use `fmap` on the tuple because it is a `Functor` on the second
argument, just like `Either`!

</details>

---

### `copyFiles` and `writeFiles`

The only thing left to do is to write the directory
content, after the processing is completed, to the newly created directory:

```hs
-- | Copy files to a directory, recording errors to stderr.
copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files = do
  let
    copyFromTo file = copyFile file (outputDir </> takeFileName file)
  void $ applyIoOnList copyFromTo files >>= filterAndReportFailures
```

Here we use `applyIoOnList` again to do something a bit more complicated,
instead of reading from a file, it copies from the input path to a newly generated
output path. Then we pass the result (which has the type `[(FilePath, Either String ())]`)
to `filterAndReportFailures` to print the errors and filter out the unsuccessful copies.
Because we are not really interested in the output of `filterAndReportFailures`,
we discard it with `void`, returning `()` as a result instead:

```hs
-- | Write files to a directory, recording errors to stderr.
writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir files = do
  let
    writeFileContent (file, content) =
      writeFile (outputDir </> file) content
  void $ applyIoOnList writeFileContent files >>= filterAndReportFailures
```

Once again, this code looks almost exactly like `copyFiles`, but the types are different.
Haskell's combination of parametric polymorphism + type class for abstractions is really
powerful, and has helped us reduce quite a bit of code.

---

This pattern of using `applyIoOnList` and then `filterAndReportFailures`
happens more than once. It might be a good candidate for refactoring. Try it!
What do you think about the resulting code? Is it easier or more difficult to
understand? Is it more modular or less? What are the pros and cons?

---

## Summary

With that, we have completed our `HsBlog.Directory` module that is responsible for converting
a directory safely. Note that the code could probably be simplified quite a bit if we
were fine with errors crashing the entire program altogether, but sometimes this is
the price we pay for robustness. It is up to you to choose what you can live with
and what not, but I hope this saga has taught you how to approach error handling
in Haskell in case you need to.

View the full module:

<details><summary>HsBlog.Directory</summary>

```hs
-- | Process multiple files and convert directories

module HsBlog.Directory
  ( convertDirectory
  , buildIndex
  )
  where

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html
import HsBlog.Convert (convert, convertStructure)

import Data.List (partition)
import Data.Traversable (for)
import Control.Monad (void, when)

import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, displayException, SomeException(..))
import System.Exit (exitFailure)
import System.FilePath
  ( takeExtension
  , takeBaseName
  , (<.>)
  , (</>)
  , takeFileName
  )
import System.Directory
  ( createDirectory
  , removeDirectoryRecursive
  , listDirectory
  , doesDirectoryExist
  , copyFile
  )

-- | Copy files from one directory to another, converting '.txt' files to
--   '.html' files in the process. Recording unsuccessful reads and writes to stderr.
--
-- May throw an exception on output directory creation.
convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory inputDir outputDir = do
  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
  createOutputDirectoryOrExit outputDir
  let
    outputHtmls = txtsToRenderedHtml filesToProcess
  copyFiles outputDir filesToCopy
  writeFiles outputDir outputHtmls
  putStrLn "Done."

------------------------------------
-- * Read directory content

-- | Returns the directory content
getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let
    (txtFiles, otherFiles) =
      partition ((== ".txt") . takeExtension) files
  txtFilesAndContent <-
    applyIoOnList readFile txtFiles >>= filterAndReportFailures
  pure $ DirContents
    { dcFilesToProcess = txtFilesAndContent
    , dcFilesToCopy = otherFiles
    }

-- | The relevant directory content for our application
data DirContents
  = DirContents
    { dcFilesToProcess :: [(FilePath, String)]
      -- ^ File paths and their content
    , dcFilesToCopy :: [FilePath]
      -- ^ Other file paths, to be copied directly
    }

------------------------------------
-- * Build index page

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files =
  let
    previews =
      map
        ( \(file, doc) ->
          case doc of
            Markup.Heading 1 heading : article ->
              Html.h_ 3 (Html.link_ file (Html.txt_ heading))
                <> foldMap convertStructure (take 2 article)
                <> Html.p_ (Html.link_ file (Html.txt_ "..."))
            _ ->
              Html.h_ 3 (Html.link_ file (Html.txt_ file))
        )
        files
  in
    Html.html_
      "Blog"
      ( Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog"))
        <> Html.h_ 2 (Html.txt_ "Posts")
        <> mconcat previews
      )

------------------------------------
-- * Conversion

-- | Convert text files to Markup, build an index, and render as html.
txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml txtFiles =
  let
    txtOutputFiles = map toOutputMarkupFile txtFiles
    index = ("index.html", buildIndex txtOutputFiles)
  in
    map (fmap Html.render) (index : map convertFile txtOutputFiles)

toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)
toOutputMarkupFile (file, content) =
  (takeBaseName file <.> "html", Markup.parse content)

convertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile (file, doc) = (file, convert file doc)

------------------------------------
-- * Output to directory

-- | Creates an output directory or terminates the program
createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit outputDir =
  whenIO
    (not <$> createOutputDirectory outputDir)
    (hPutStrLn stderr "Cancelled." *> exitFailure)

-- | Creates the output directory.
--   Returns whether the directory was created or not.
createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory dir = do
  dirExists <- doesDirectoryExist dir
  create <-
    if dirExists
      then do
        override <- confirm "Output directory exists. Override?"
        when override (removeDirectoryRecursive dir)
        pure override
      else
        pure True
  when create (createDirectory dir)
  pure create

-- | Copy files to a directory, recording errors to stderr.
copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files = do
  let
    copyFromTo file = copyFile file (outputDir </> takeFileName file)
  void $ applyIoOnList copyFromTo files >>= filterAndReportFailures

-- | Write files to a directory, recording errors to stderr.
writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir files = do
  let
    writeFileContent (file, content) =
      writeFile (outputDir </> file) content
  void $ applyIoOnList writeFileContent files >>= filterAndReportFailures

------------------------------------
-- * IO work and handling errors

-- | Try to apply an IO function on a list of values, document successes and failures
applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList action inputs = do
  for inputs $ \input -> do
    maybeResult <-
      catch
        (Right <$> action input)
        ( \(SomeException e) -> do
          pure $ Left (displayException e)
        )
    pure (input, maybeResult)

-- | Filter out unsuccessful operations on files and report errors to stderr.
filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap $ \(file, contentOrErr) ->
    case contentOrErr of
      Left err -> do
        hPutStrLn stderr err
        pure []
      Right content ->
        pure [(file, content)]

------------------------------------
-- * Utilities

confirm :: String -> IO Bool
confirm question = do
  putStrLn (question <> " (y/n)")
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. Use y or n."
      confirm question

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  if result
    then action
    else pure ()
```

</details>
