# Generating documentation

There are [many ways](https://documentation.divio.com/)
to help others to get started with our projects and libraries.
For example, we can write tutorials, provide runnable examples,
describe the internals of the system, and create an API reference.

In this chapter we will focus on generating API reference pages (the kind that can be seen on Hackage)
from annotated Haskell source code using [Haddock](https://www.haskell.org/haddock).

## Running Haddock

We can generate API reference pages (a.k.a. haddocks in the Haskell world) for our project
using our favorite package manager:

### Cabal

We can run `cabal haddock` to generate haddocks:

```sh
➜ cabal haddock
Resolving dependencies...
Build profile: -w ghc-9.0.1 -O1
In order, the following will be built (use -v for more details):
 - hs-blog-0.1.0.0 (lib) (first run)
Configuring library for hs-blog-0.1.0.0..
Preprocessing library for hs-blog-0.1.0.0..
Running Haddock on library for hs-blog-0.1.0.0..
Haddock coverage:
   0% (  0 /  3) in 'HsBlog.Env'
  Missing documentation for:
    Module header
    Env (src/HsBlog/Env.hs:3)
    defaultEnv (src/HsBlog/Env.hs:10)
  21% (  7 / 33) in 'HsBlog.Html.Internal'
  Missing documentation for:
    Module header
    Html (src/HsBlog/Html/Internal.hs:8)
...
Documentation created:
/tmp/learn-haskell-blog-generator/dist-newstyle/build/x86_64-linux/ghc-9.0.1/hs-blog-0.1.0.0/doc/html/hs-blog/index.html
```

Cabal and Haddock will build our project and generate HTML pages for us at:

```html
./dist-newstyle/build/<platform>/<compiler>/<package>-<version>/doc/html/<package>/
```

We can then open the `index.html` file from that directory in a web browser and view our package documentation.

### Stack

We can run `stack haddock` to generate haddocks:

```sh
➜ stack haddock
...
hs-blog> build (lib + exe)
Preprocessing library for hs-blog-0.1.0.0..
Building library for hs-blog-0.1.0.0..
[1 of 7] Compiling HsBlog.Env
[2 of 7] Compiling HsBlog.Html.Internal
...
hs-blog> haddock
Preprocessing library for hs-blog-0.1.0.0..
Running Haddock on library for hs-blog-0.1.0.0..
Haddock coverage:
   0% (  0 /  3) in 'HsBlog.Env'
  Missing documentation for:
    Module header
    Env (src/HsBlog/Env.hs:3)
    defaultEnv (src/HsBlog/Env.hs:10)
  21% (  7 / 33) in 'HsBlog.Html.Internal'
  Missing documentation for:
    Module header
    Html (src/HsBlog/Html/Internal.hs:8)
...
Documentation created:
.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/doc/html/hs-blog/index.html,
.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/doc/html/hs-blog/hs-blog.txt
Preprocessing executable 'hs-blog-gen' for hs-blog-0.1.0.0..
...
```

Stack and Haddock will build our project and generate HTML pages for us at:

```html
./.stack-work/dist/<platform>/Cabal-<version>/doc/html/<package>/
```

We can then open the `index.html` file from that directory in a web browser and view our package documentation.

### Haddock coverage

Haddock will also output a coverage report when run, and will mention user-exposed constructs which are missing
documentation. These constructs could be module headers, types, data constructors, type classes, functions, values, etc.

For example:

```hs
Haddock coverage:
...
   0% (  0 /  3) in 'HsBlog.Convert'
  Missing documentation for:
    Module header
    convert (src/HsBlog/Convert.hs:8)
    convertStructure (src/HsBlog/Convert.hs:23)
  67% (  2 /  3) in 'HsBlog.Directory'
  Missing documentation for:
    buildIndex (src/HsBlog/Directory.hs:80)
...
```

We can see that we did not document the `HsBlog.Convert` at all, and we are missing
documentation for the module header, the `convert` function and the `convertStructure` function.

On the other hand, it seems that we do currently have some documentation written for the `HsBlog.Directory`
module! We'll see why, but first - try to generate haddocks, see the module hierarchy, browse around
the different modules, follow the links of the types, imagine what this API reference could look like,
and let's see how we can improve it.

## Haddock markup

Haddock builds the API reference pages by building our project, examining the exported modules
and their exported definitions, and grabbing source code comments written in special markup format.

Let's take a quick look at this markup format. We will go over a few important bits,
but if you'd like to learn more, a complete guide for Haddock markup can be found in the
[Haddock documentation](https://haskell-haddock.readthedocs.io/en/latest/markup.html).


### Documenting definitions

All haddock annotations appear as part of regular Haskell comments.
They can be used with both single line form (`--`) and multi-line form (`{-` and `-}`).
The placement of a comment block and the haddock marker determine to which Haskell
definition the haddock string is attached.

We can annotate a Haskell definition by writing a comment block prefixed with `|` *before*
the definition, or by writing a comment block prefixed with `^` *after* the definition.

For example:

```hs
-- | Construct an HTML page from a `Head`
--   and a `Structure`.
html_
  :: Head -- ^ Represents the @\<head\>@ section in an HTML file
  -> Structure -- ^ Represents the @\<body\>@ section in an HTML file
  -> Html
html_ = ...
...
```

Here's another example:

```hs
{- | Represents a single markup structure. Such as:

- A paragraph
- An unordered list
- A code block
-}
data Structure
  = Heading Natural String
  -- ^ A section heading with a level
  | Paragraph String
  -- ^ A paragraph
  | UnorderedList [String]
  -- ^ An unordered list of strings
  | OrderedList [String]
  -- ^ An ordered list of strings
  | CodeBlock [String]
  -- ^ A code block
```

And another:

```hs
{- | Markup to HTML conversion module.

This module handles converting documents written in our custom
Markup language into HTML pages.
-}
module HsBlog.Convert where
```

As you can see, `|` and `^` can be used to document functions, function arguments,
types, data constructors, modules, and more. They are probably the most important
Haddock annotations to remember (and even then, `|` alone will suffice).

> **Tip**: Annotate the modules, types, and the top-level definitions
> which are exported from your project
> with some high-level description of what they are used for (at the very least).
>
> Your users and collaborators will thank you!

### Section headings

We can separate our module into sections by adding headings.
Headings are comments which are prefixed with a number of `*` (just like in our markup language).

For example:

```hs
-- * HTML EDSL

html_ :: Head -> Structure -> Html
html_ = ...

-- ** Structure

p_ :: Content -> Structure
p_ = ..

h_ :: Content -> Structure
h_ = ..

...

-- ** Content

txt_ :: String -> Content
txt_ = ...

link_ :: FilePath -> Content -> Content
link_ = ...
```

It is also possible to add headings to the export list instead:

```hs
module HsBlog.Html
  ( -- * HTML EDSL
    Html
  , html_

    -- ** Combinators used to construct the @\<head\>@ section
  , Head
  , title_
  , stylesheet_
  , meta_

    -- ** Combinators used to construct the @\<body\>@ section
  , Structure
  , p_
  , h_
  , ul_
  , ol_
  , code_

    -- ** Combinators used to construct content inside structures
  , Content
  , txt_
  , img_
  , link_
  , b_
  , i_

    -- ** Render HTML to String
  , render
  )
  where
```

Separating parts of the module into sections helps keeping the important things together
and Haddock will create a table-of-contents at the top of a module page for us as well.

Sometimes it's also easier to figure out whether a module should be split into multiple
modules or not after splitting it into sections using headings.

---

**Exercise**: Try to re-arrange the modules in our project to your liking and add headings to sections.

---

### Formatting

As we saw earlier, we can also add formatting in the content of our comments.
For example, we can:

- Hyperlink identifiers by surrounding them with `` ` ``

  For example: `` `Heading` ``
- Write `monospaced text` by surrounding it with `@`

  For example: `@Paragraph "Hello"@`
- Add _emphasis_ to text by surrounding it with `/`

  For example: `/this is emphasised/`
- Add __bold__ to text by surrounding it with `__`

  For example: `__this is bold__`

### More

In this chapter we've covered the basics of the Haddock markup language.
If you'd like to know more, the [Haddock markup guide](https://haskell-haddock.readthedocs.io/en/latest/markup.html)
contains information on how to create even more interesting documentation structures, such as
code blocks, grid tables, images and examples.

## Summary

We've briefly covered one aspect of documenting Haskell programs:
using Haddock to generate informative API reference pages created from source code
comments which are annotated with Haddock markup.

While API references are incredibly valuable, remember that there are other forms of
documentation that can help your users get started quickly, such as examples and tutorials.


---

**Exercise**: Add haddock annotation to the top-level definitions in our project and test your understanding
of the program and the various parts - sometimes the best way to learn something is to try explaining it!

---
