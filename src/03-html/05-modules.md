# Preventing incorrect use with modules

In this section we will move the HTML generation library to its own module.

## Modules

Each Haskell source file is a module. The module name should have the
same name as the source file and should start with a capital
letter. Sub-directories should also be part of the name and we use `.`
to denote a sub-directory. We'll see that in the next section.

The only exception to the rule are entry points to the program -
modules with the name 'Main' that define `main` in them. Their source
file names could have any name they want.

A module declaration looks like this:

```hs
module <module-name>
  ( <export-list>
  )
  where
```

The export list can be omitted if you want to export everything
defined in the module, but we don't. We will list exactly the
functions and types we want to export. This will give us control
on how people can use our tiny library.

We will create a new source file named `Html.hs` and add the following
module declaration code at the top of the file:

```hs
module Html
  ( Html
  , Title
  , Structure
  , html_
  , p_
  , h1_
  , append_
  , render
  )
  where
```

Note that we do not export:

1. The constructors for our new types, only the types themselves.
   If we wanted to export the constructors as well we would've written
   `Html(Html)` or `Html(..)`. This way the user cannot create their own
   `Structure` by writing `Structure "Hello"`.

2. Internal functions used by the library, such as `el` and `getStructureString`.

And we will also move the HTML related functions from our `hello.hs` file
to this new `Html.hs` file:

```hs
newtype Html
  = Html String

newtype Structure
  = Structure String

type Title
  = String

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el "html"
      ( el "head" (el "title" title)
        <> el "body" (getStructureString content)
      )
    )

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

append_ :: Structure -> Structure -> Structure
append_ c1 c2 =
  Structure (getStructureString c1 <> getStructureString c2)

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

render :: Html -> String
render html =
  case html of
    Html str -> str
```

Now, anyone importing our module (using the `import` statement
below module declarations but above any other
declaration), will only be able to import what we export.

Add the following code at the top of the `hello.hs` file:

```hs
import Html
```

The `hello.hs` file should now look like this:

```hs
-- hello.hs

import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My title"
    ( append_
      (h1_ "Heading")
      ( append_
        (p_ "Paragraph #1")
        (p_ "Paragraph #2")
      )
    )
```

And the `Html.hs` file should look like this:

```hs
-- Html.hs

module Html
  ( Html
  , Title
  , Structure
  , html_
  , p_
  , h1_
  , append_
  , render
  )
  where

newtype Html
  = Html String

newtype Structure
  = Structure String

type Title
  = String

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el "html"
      ( el "head" (el "title" title)
        <> el "body" (getStructureString content)
      )
    )

p_ :: String -> Structure
p_ = Structure . el "p"

h1_ :: String -> Structure
h1_ = Structure . el "h1"

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

append_ :: Structure -> Structure -> Structure
append_ c1 c2 =
  Structure (getStructureString c1 <> getStructureString c2)

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

render :: Html -> String
render html =
  case html of
    Html str -> str
```

> As an aside, you might have noticed that I've decided to suffix the functions used to
> construct HTML values with an undescore (`_`). This mostly an aesthetic decision which,
> in my opinion, makes the EDSL easier to recognize,
> but it is also useful to avoid name clashes with
> functions defined in the Haskell standard library, such as `head`.
> I took this idea from a Haskell HTML library named `lucid`!
