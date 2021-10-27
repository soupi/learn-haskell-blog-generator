# Passing environment variables

We'd like to add some sort of an environment to keep general information on
the blog for various processing. Things like the blog name, stylesheet
location, and so on.

## Environment

We can represent our environment as a record data type and build it from user input.
The user input can be a from command-line arguments, a configuration file,
or something else.

```hs
module HsBlog.Env where

data Env
  = Env
    { eBlogName :: String
    , eStylesheetPath :: FilePath
    }
  deriving Show

defaultEnv :: Env
defaultEnv = Env "My Blog" "style.css"
```

After filling this record with the requested information, we can pass it as
input to any function that might need it. This is a simple approach that can definitely
work for small projects. But sometimes when the project gets bigger and many
nested functions need the same information, threading the environment can get
tedious.

There is an alternative solution to threading the environment as input to functions,
and that is using the
[`ReaderT`](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html#g:2)
type from the `mtl` (or `transformers`) package.

### ReaderT

```hs
newType ReaderT r m a = ReaderT (r -> m a)
```

`ReaderT` is another *monad transformer* like `ExceptT`, which means
that it also has an instance of `Functor`, `Applicative`, `Monad` and `MonadTrans`.

As we can see in the definition, `ReaderT` is *a newtype* over a function that takes
some value of type `r`, and returns a value of type `m a`. The `r` usually
represents the environment we want to share between different functions that we want to
compose together, and the `m a` represents the underlying result that we return.
The `m` could be any type that implements `Monad` that we are familiar with.
Usually goes well with `IO` or `Identity`, depending if we want to share
an environment between effectful or uneffectful computations.

What `ReaderT` does is *carry* a value of type `r` and pass it around to
other functions when we use the `Applicative` and `Monad` interfaces so that
we don't have to pass the value around manually. And when we want to grab
the `r` and use it, all we have to do is `ask`.

For our case, this means that instead of Passing around `Env`, we can instead
convert our functions to use `ReaderT` - those that are uneffectful and don't use
`IO`, can return `ReaderT Env Identity a`  instead of `a` (or the simplified version, `Reader Env a`),
and those that are effectful can return `ReaderT Env IO a` instead of `IO a`.

Note, as we've said before, `Functor`, `Applicative` and `Monad` all expect the type
that implements their interface to have the kind `* -> *`.
This means that it is `ReaderT r m` implements this interfaces, and when we compose functions with
`<*>` or `>>=` we replace the `f` or `m` in their type signature with `ReaderT r m`.

This means that, like with `Either e` when we had compose functions with the same error type,
so is with `ReaderT r m` - we have to compose functions with the same `r` type and same
`m` type, so we can't mix different environment types or different underlying `m` types.

We're going to use a specialized version of `ReaderT` that uses a specific `m` = `Identity`
called [`Reader`](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html#g:2).
The `Control.Monad.Reader` provides an alias: `Reader r a = ReaderT r Identity a`.

> If the idea behind `ReaderT` is still a bit fuzzy to you and you want
> to get a better understanding of how `ReaderT` works,
> try doing the following exercise:
> 1. Choose an `Applicative` or `Monad` interface function, I recommend `liftA2`,
>    and specialize it's type signature by replacing `f` (or `m`) with a concrete `ReaderT` type such as
>    `ReaderT Int IO`.
> 2. Unpack the `ReaderT` newtype, replacing `ReaderT Int IO t` with `Int -> IO t`.
> 3. Implement this specialized version of the function you've chosen
>
> <details><summary>Solution for liftA2</summary>
>
> ```hs
> liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
> ```
>
> <details><summary>Solution for (1)</summary>
>
> ```hs
> -- Specialize: replace `f` with `ReaderT Env IO`
> liftA2 :: (a -> b -> c) -> ReaderT Env IO a -> ReaderT Env IO b -> ReaderT Env IO c
> ```
>
> </details>
>
> <details><summary>Solution for (2)</summary>
>
> ```hs
> -- Unpack the newtype, replacing `ReaderT Env IO a` with `Env -> IO a`
> liftA2 :: (a -> b -> c) -> (Env -> IO a) -> (Env -> IO b) -> (Env -> IO c)
> ```
>
> </details>
>
> <details><summary>Solution for (3)</summary>
>
> ```hs
> specialLiftA2 :: (a -> b -> c) -> (Env -> IO a) -> (Env -> IO b) -> (Env -> IO c)
> specialLiftA2 combine funcA funcB env =
>   liftA2 combine (funcA env) (funcB env)
> ```
>
> Notice how the job of our special `liftA2` for `ReaderT` is to supply the
> two functions with `env`, and then use the `liftA2`
> implementation of the underlying `m` type (in our case `IO`) to do the rest of the work.
> Does it look like we're adding a capability on top of a different `m`?
> That's the idea behind monad transformers.
>
> </details>
> </details>

### How to use Reader

#### Defining a function

Instead of defining a function like this:

```hs
txtsToRenderedHtml :: Env -> [(FilePath, String)] -> [(FilePath, String)]
```

We define it like this:

```hs
txtsToRenderedHtml :: [(FilePath, String)] -> Reader Env [(FilePath, String)]
```

Now that our code uses `Reader`, we have to accommodate that in the way we write our function:

Before:

```hs
txtsToRenderedHtml :: Env -> [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml env txtFiles =
 let
   txtOutputFiles = map toOutputMarkupFile txtFiles
   index = ("index.html", buildIndex env txtOutputFiles)
   htmlPages = map (convertFile env) txtOutputFiles
 in
   map (fmap Html.render) (index : htmlPages)
```

Note how we needed to thread the `env` to the other functions that use it.

After:

```hs
txtsToRenderedHtml :: [(FilePath, String)] -> Reader Env [(FilePath, String)]
txtsToRenderedHtml txtFiles = do
  let
    txtOutputFiles = map toOutputMarkupFile txtFiles
  index <- (,) "index.html" <$> buildIndex txtOutputFiles
  htmlPages <- traverse convertFile txtOutputFiles
  pure $ map (fmap Html.render) (index : htmlPages)
```

Note how we use *do notation* now, and *instead of threading* `env` around we *compose*
the relevant functions, `buildIndex` and `convertFile`, we use the type classes
interfaces to compose the functions. Note how we needed to `fmap` over `buildIndex`
to add the output file we needed with the tuple, and how we needed to use `traverse` instead
of `map` to compose the various `Reader` values `convertFile` will produce.

### Extracting `Env`

When we want to use our `Env`, we need to *extract* it from the `Reader`.
We can do it with:

```hs
ask :: ReaderT r m r
```

Which yanks the `r` from the `Reader` - we can extract with `>>=` or `<-` in do notation.
See the comparison:

Before:

```hs
convertFile :: Env -> (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile env (file, doc) =
  (file, convert env (takeBaseName file) doc)
```

After:

```hs
convertFile :: (FilePath, Markup.Document) -> Reader Env (FilePath, Html.Html)
convertFile (file, doc) = do
  env <- ask
  pure (file, convert env (takeBaseName file) doc)
```

> Note: we didn't change `convert` to use `Reader` because it is a user facing API for our
> library. By providing simpler interface we give our users more flexibility - what if
> they already use `ReaderT` with a different environment, or a different underlying
> `m`? Providing a simple function argument passing interface is preferred in this case.

### Run a `Reader`

Similar to handling the errors with `Either`, At some point we need to supply the environment to
a computation that uses `Reader`, and extract the result from the computation.
We can do that with the functions `runReader` and `runReaderT`:

```hs
runReader :: Reader r a -> (r -> a)

runReaderT :: ReaderT r m a -> (r -> m a)
```

These function converts a `Reader` or `ReaderT` to a function that takes and `r`.
Then we can pass the initial environment to that function:

```hs
convertDirectory :: Env -> FilePath -> FilePath -> IO ()
convertDirectory env inputDir outputDir = do
  DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
  createOutputDirectoryOrExit outputDir
  let
    outputHtmls = runReader (txtsToRenderedHtml filesToProcess) env
  copyFiles outputDir filesToCopy
  writeFiles outputDir outputHtmls
  putStrLn "Done."
```

See the `let outputHtmls`part.

### Using `Env` in our logic code

One thing we haven't talked about yet is actually using the environment in the `convert`
function to generate the pages we want. And actually, we don't even have the ability to add
stylesheets to our HTML EDSL at the moment! We need to go back and extend it. Let's do all
that now:

---

Since stylesheets go in the `head` element, perhaps it's a good idea to create an additional
`newtype` like `Structure` for `Header` information? Things like title, stylesheet,
and even meta elements can be composed together just like we did for `Structure`
to build the `Header`!

1. Do it now: extend our HTML library to include headers and add 3 functions:
   `title_` for titles, `stylesheet_` for stylesheets, and `meta_` for meta elements
   like [twitter cards](https://developer.twitter.com/en/docs/twitter-for-websites/cards/overview/abouts-cards).

   <details><summary>Solution</summary>
   
     <details><summary>src/HsBlog/Html.hs</summary>
     
     ```hs
     -- Html.hs
     
     module HsBlog.Html
       ( Html
       , Header
       , title_
       , stylesheet_
       , meta_
       , Structure
       , html_
       , p_
       , h_
       , h1_
       , ul_
       , ol_
       , code_
	   , Content
       , txt_
       , img_
       , link_
       , b_
       , i_
       , render
       )
       where
     
     import HsBlog.Html.Internal
     ```
     
     </details>
     
     <details><summary>src/HsBlog/Html/Internal.hs</summary>
     
     ```hs
     newtype Header
       = Header String
     
     -- * EDSL
     
     html_ :: Header -> Structure -> Html
     html_ (Header header) content =
       Html
         ( el "html"
           ( el "head" header
             <> el "body" (getStructureString content)
           )
         )
     
     -- * Header
     
     title_ :: String -> Header
     title_ = Header . el "title" . escape
     
     stylesheet_ :: FilePath -> Header
     stylesheet_ path =
       Header $ "<link rel=\"stylesheet\" type=\"text/css\" href=\"" <> escape path <> "\">"
     
     meta_ :: String -> String -> Header
     meta_ name content =
       Header $ "<meta name=\"" <> escape name <> "\" content=\"" <> escape content <> "\">"
     
     instance Semigroup Header where
       (<>) (Header h1) (Header h2) =
         Header (h1 <> h2)
     
     instance Monoid Header where
       mempty = Header ""
     ```
     
     </details>
   
   </details>

2. Fix `convert` and `buildIndex` to use the new API. Note: `buildIndex` should return
   `Reader`!


   <details><summary>Solution</summary>
   
     <details><summary>src/HsBlog/Convert.hs</summary>
     
     ```hs
     import HsBlog.Env (Env(..))
     
     convert :: Env -> String -> Markup.Document -> Html.Html
     convert env title doc =
       let
         header =
           Html.title_ (eBlogName env <> " - " <> title)
             <> Html.stylesheet_ (eStylesheetPath env)
         article =
           foldMap convertStructure doc
         websiteTitle =
           Html.h1_ (Html.link_ "index.html" $ Html.txt_ $ eBlogName env)
         body =
           websiteTitle <> article
       in
         Html.html_ header body
     ```
     
     </details>
     
     <details><summary>src/HsBlog/Directory.hs</summary>
     
     ```hs
     buildIndex :: [(FilePath, Markup.Document)] -> Reader Env Html.Html
     buildIndex files = do
       env <- ask
       let
         previews =
           map
             ( \(file, doc) ->
               case doc of
                 Markup.Header 1 header : article ->
                   Html.h_ 3 (Html.link_ file (Html.txt_ header))
                     <> foldMap convertStructure (take 2 article)
                     <> Html.p_ (Html.link_ file (Html.txt_ "..."))
                 _ ->
                   Html.h_ 3 (Html.link_ file (Html.txt_ file))
             )
             files
       pure $ Html.html_
           ( Html.title_ (eBlogName env)
             <> Html.stylesheet_ (eStylesheetPath env)
           )
           ( Html.h1_ (Html.link_ "index.html" (Html.txt_ "Blog"))
             <> Html.h_ 2 (Html.txt_ "Posts")
             <> mconcat previews
           )
     ```
     
     </details>
   
   </details>

3. Create a command-line parser for `Env`, attach it to the `convert-dir` command,
   and pass the result it to the `convertDirectory` function.

   <details><summary>Solution</summary>
   
     <details><summary>src/HsBlog.hs</summary>
     
     ```hs
     import HsBlog.Env (defaultEnv)
     
     convertSingle :: String -> Handle -> Handle -> IO ()

     process :: String -> String -> String
     process title = Html.render . convert defaultEnv title . Markup.parse
     ```
     
     </details>
     
     
     <details><summary>app/OptParse.hs</summary>
     
     ```hs
     import HsBlog.Env
     
     ------------------------------------------------
     -- * Our command-line options model

     -- | Model
     data Options
       = ConvertSingle SingleInput SingleOutput
       | ConvertDir FilePath FilePath Env
       deriving Show
     
     ------------------------------------------------
     -- * Directory conversion parser

     pConvertDir :: Parser Options
     pConvertDir =
       ConvertDir <$> pInputDir <*> pOutputDir <*> pEnv
     
     -- | Parser for blog environment
     pEnv :: Parser Env
     pEnv =
       Env <$> pBlogName <*> pStylesheet
     
     -- | Blog name parser
     pBlogName :: Parser String
     pBlogName =
       strOption
         ( long "name"
           <> short 'N'
           <> metavar "STRING"
           <> help "Blog name"
           <> value (eBlogName defaultEnv)
           <> showDefault
         )
     
     -- | Stylesheet parser
     pStylesheet :: Parser String
     pStylesheet =
       strOption
         ( long "style"
           <> short 'S'
           <> metavar "FILE"
           <> help "Stylesheet filename"
           <> value (eStylesheetPath defaultEnv)
           <> showDefault
         )
     
     ```

     </details>
     
     <details><summary>app/Main.hs</summary>
     
     ```hs
     main :: IO ()
     main = do
       options <- parse
       case options of
         ConvertDir input output env ->
           HsBlog.convertDirectory env input output
     
         ...
     ```
     
     </details>
   
   </details>



---

### Summary

Which version do you like better? Manually passing arguments, or using `Reader`?

To me, it is not clear that the second version with `Reader` is better than the first
with explicit argument passing.

Using `Reader` and `ReaderT` makes our code a little less friendly toward beginners
that are not yet familiar with these concepts and techniques, and we don't see
(in this case) much benefit.

As programs grow larger, techniques like using `Reader` become more attractive to use.
For our relatively small example, using `Reader` might not be appropriate.
I've included it in this book because it is an important technique to have in our
arsenal and I wanted to demonstrate it.

It is important to weigh the benefits and costs of using advanced techniques,
and it's often better to try and get away with simpler techniques if we can.

> You can view the git commit of
> [the changes we've made](https://github.com/soupi/learn-haskell-blog-generator/commit/fe72df16c1af822484cec546b892526f96bf9d59)
> and the [code up until now](https://github.com/soupi/learn-haskell-blog-generator/tree/code-after-reader).
