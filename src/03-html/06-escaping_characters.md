# Escaping characters

Now that `Html` has its own source file and module, and creating
HTML code can be done only via the functions we exported,
we can also handle user input that may contain characters
that may conflict with our meta language, HTML,
such as `<` and `>` which are used for creating HTML tags.

We can convert these characters into different strings that HTML can handle.

See [Stack overflow question](https://stackoverflow.com/questions/7381974/which-characters-need-to-be-escaped-in-html)
for a list of characters we need to escape.

Let's create a new function called `escape`:

```hs
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

In `escape` we see a few new things:

1. Let expressions: we can define local names using this syntax:

   ```hs
   let
     <name> = <expression>
   in
     <expression>
   ```

   This will make `<name>` available as a variable `in` the second `<expression>`.

2. Pattern matching with multiple patterns: we match on different
   characters and convert them to a string. Note that `_` is a "catch
   all" pattern that will always succeed.

3. Two new functions: `map` and `concat`, we'll talk about these more in depth

4. That the syntax highlighting broke a bit for this snippet for some reason. Don't worry about it.

## Linked lists briefly

Linked lists are very common data structures in Haskell, so common that
they have their own special syntax:

1. The list types are denoted with brackets and inside them is the type of the element. For example:
   - `[Int]` - a list of integers
   - `[Char]` - a list of characters
   - `[String]` - a list of strings
   - `[[String]]` - a list of a list of strings
   - `[a]` - a list of any single type (all elements must be of the same type)
2. An empty list is written like this: `[]`
3. Prepending an element to a list is done with the operator `:` (pronounced cons) which is right-associative (like `->`).
   For example: `1 : []`, or `1 : 2 : 3 : []`.
4. The above lists can also be written like this: `[1]` and `[1, 2, 3]`.

Also, Strings are linked lists of characters - String is defined as:
`type String = [Char]`, so we can use them the same way we use lists.

> Do note, however, that linked lists, despite their convenience, are often
> not the right tool for the job. They are not particularly space efficient
> and are slow for appending, random access and more. That also makes `String`
> a lot less efficient than what it could be. And I generally recommend using a
> different string type, `Text`, instead, which is available in an external package.
> We will talk about lists, `Text`, and other data structures in the future!

We can implement our own operations on lists by using pattern matching and recursion.
And we'll touch on this subject later when talking about ADTs.

For now, we will use the various functions found in the
[Data.List](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-List.html) module.
Specifically, [map](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-List.html#v:map)
and [concat](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-List.html#v:concat).

### `map`

Using `map` we can apply a function to each of the elements in a list. Its type signature is:

```hs
map :: (a -> b) -> [a] -> [b]
```

For example:

```hs
map not [False, True, False] == [True, False, True]
```

Or as can be seen in our `escape` function, this can help us escape each character:

```hs
map escapeChar ['<','h','1','>'] == ["&lt;","h","1","&gt;"]
```

However, note that the `escapeChar` has the type `Char -> String`,
so the result type of `map escapeChar ['<','h','1','>']` is `[String]`,
and what we really want is a `String` and not `[String]`.

This is where `concat` enters the picture to help us flatten the list.

### `concat`

`concat` has the type:

```hs
concat :: [[a]] -> [a]
```

It flattens a list of list of something into a list of something.
In our case it will flatten `[String]` into `String`, remember that
`String` is a **type alias** for `[Char]`, so we actually have
`[[Char]] -> [Char]`.

## GHCi

One way we can quickly see our code in action is using the interactive development environment **GHCi**.
Running `ghci` will open an interactive prompt where Haskell expressions can be written and
evaluated. This is called a "Read-Evaluate-Print Loop" (for short - REPL).

For example:

```
ghci> 1 + 1
2
ghci> putStrLn "Hello, world!"
Hello, world!
```

We can define new names:

```
ghci> double x = x + x
ghci> double 2
4
```

We can write multi-line code by surrounding it with `:{` and `:}`:

```
ghci> :{
| escape :: String -> String
| escape =
|   let
|     escapeChar c =
|       case c of
|         '<' -> "&lt;"
|         '>' -> "&gt;"
|         '&' -> "&amp;"
|         '"' -> "&quot;"
|         '\'' -> "&#39;"
|         _ -> [c]
|   in
|     concat . map escapeChar
| :}

ghci> escape "<html>"
"&lt;html&gt;"

```

We can import Haskell source files using the `:load` command (`:l` for short):

```
ghci> :load Html.hs
[1 of 1] Compiling Html    ( Html.hs, interpreted )
Ok, one module loaded.
ghci> render (html_ "<title>" (p_ "<body>"))
"<html><head><title>&lt;title&gt;</title></head><body><p>&lt;body&gt;</p></body></html>"
```

As well as import library modules:

```
ghci> import Data.Bits
ghci> shiftL 32 1
64
ghci> clearBit 33 0
32
```

We can even ask the type of an expression using the `:type` command
(`:t` for short):

```
λ> :type escape
escape :: String -> String
```

To exit `ghci`, use the `:quit` command (or `:q` for short)

```
ghci> :quit
Leaving GHCi.
```

GHCi is a very useful tool for quick experiments and exploration.
We've seen a couple of examples of that above - passing the string `"<html>"` to our
`escape` function returns the string `"&lt;html&gt;"`, which can be rendered by
a browser as `<html>` instead of an HTML tag.

If you are having a hard time figuring out what a particular function does, consider
testing it in GHCi - pass it different inputs, and see if it matches your expectations.
Concrete examples of running code can aid a lot in understanding it!

> If you'd like to learn more about GHCi, you can find a more thorough introduction in the
> [GHC user guide](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html).

## Escaping

---

The user of our library can currently only supply strings in a few places:

1. Page title
2. Paragraphs
3. Headings

We can apply our escape function at these places before doing anything else with it.
That way all HTML constructions are safe.

Try adding the escaping function in those places.

<details>
  <summary>Solution</summary>

```hs
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
```

</details>

---

<details>
  <summary><b>Our revised Html.hs</b></summary>

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

Try constructing an invalid HTML in `hello.hs` to see if this works or not!

Now we can use our tiny HTML library safely. But what if the user
wants to use our library with a valid use case we didn't think about, for
example adding unordered lists? We are completely blocking them from
extending our library. We'll talk about this next.
