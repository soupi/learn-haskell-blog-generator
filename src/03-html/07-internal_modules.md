# Exposing internal functionality (Internal modules)

We have now built a very small but convenient and safe way to write
HTML code in Haskell. This is something that we could (potentially)
publish as a *library* and share with the world by uploading it
to a package repository such as [Hackage](https://hackage.haskell.org/).
Users who are interested in our library could use a package manager
to include our library in their project and build their own HTML pages
with it.

It is important to note that users are building their project against
the API that we expose to them, and the package manager doesn't generally
provide access to the source code, so they can't, for example,
modify the `Html` module (that we expose) in their project directly
without jumping through some hoops.

Because we wanted our `Html` EDSL to be safe, we **hid the internal
implementation from the user**, and the only way to interact with the
library is via the API we provide.

This provides the safety we wanted to provide, but in this case it also
*blocks* the user from extending our library *in their own project* with
things we haven't implemented yet, such as lists or code blocks.

When a user runs into trouble with a library (such as missing features)
the best course of action usually is to open an issue in the repository or
submit a pull request, but sometimes the user needs things to work *now*.

We admit that we are not perfect and can't think of all use cases for our
library. Sometimes the restrictions we add are too great and may limit
the usage of advanced users that know how things work under the hood and
need certain functionality in order to use our library.

### Internal modules

For that we can expose internal modules to provide some flexibility for
advanced users. Internal modules are not a language concept but
rather a (fairly common) design pattern (or idiom) in Haskell.

Internal modules are simply modules named `<something>.Internal`,
which export all of the functionality and implementation details in that module.

Instead of writing the implementation in (for example) the `Html` module,
we write it in the `Html.Internal` module, which will export everything.
Then we will import that module in the `Html` module, and write an explicit export list
to only export the API we'd like to export (as before).

`Internal` modules are considered unstable and risky to use by convention.
If you end up using one yourself when using an external Haskell library,
make sure to open a ticket in the library's repository after the storm has passed!

### Let's make the changes

We will create a new directory named `Html` and inside it a new file
named `Internal.hs`. The name of this module should be `Html.Internal`.

This module will contain all of the code we previously had in the `Html`
module, but **we will change the module declaration in `Html.Internal`
and _omit_ the export list**:

```hs
-- Html/Internal.hs

module Html.Internal where

...
```

And now in `Html.hs`, we will remove the code that we moved to `Html/Internal.hs`
and in its stead we'll import the internal module:

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

import Html.Internal
```

Now, users of our library can still import `Html` and safely use our library,
but if they run into trouble and have a dire need to implement unordered lists
to work with our library, they could always work with `Html.Internal` instead.

<details>
  <summary><b>Our revised Html.hs and Html/Internal.hs</b></summary>

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

import Html.Internal
```

```hs
-- Html/Internal.hs

module Html.Internal where

-- * Types

newtype Html
  = Html String

newtype Structure
  = Structure String

type Title
  = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el "html"
      ( el "head" (el "title" (escape title))
        <> el "body" (getStructureString content)
      )
    )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

append_ :: Structure -> Structure -> Structure
append_ c1 c2 =
  Structure (getStructureString c1 <> getStructureString c2)

-- * Render

render :: Html -> String
render html =
  case html of
    Html str -> str

-- * Utilities

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

escape :: String -> String
escape =
  let
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
  in
    concat . map escapeChar
```

</details>


## Summary

For our particular project, `Internal` modules aren't necessary.
Because our project and the source code for the HTML EDSL are
part of the same project, and we have access to the `Html`
module directly, we can always go and edit it if we want
(and we are going to do that throughout the book).

However, if we were planning to release our HTML EDSL as a *library*
for other developers to use, it would be nice
to also expose the internal implementation as an `Internal`
module. Just so we can save some trouble for potential users!

We will see how to create a package from our source code in a later chapter.
