# Parsing markup part 02 (Pattern matching)

## Maybe

Previously, when we talked about partial functions, we mentioned that one way to avoid
writing partial functions is to encode the absence of a result using `Maybe`:

```hs
data Maybe a
    = Nothing
    | Just a
```

`Maybe` is a data type from the standard library (named [base](https://hackage.haskell.org/package/base))
that is used to add an additional value to a type: the absence of a value.
For example, `Maybe Bool` has three values,
two with the `Just` constructor to represent regular boolean values
(`Just True` and `Just False`) and another value, `Nothing` to represent
the absence of a boolean value.

We can use this to encode the result of `head`, a function that promises to return
the first element of a list, without creating a partial function:

```hs
head :: [a] -> Maybe a
```

This way, when the list is empty, we can return `Nothing`, and when it has at least
one element, we can return `Just <first element>`. This function can be found in
the [Data.Maybe](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Maybe.html)
module under the name
[listToMaybe](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Maybe.html#v:listToMaybe).

In order to consume values of type `Maybe <something>`, and other types created with
`data`, we can use pattern matching.

## Pattern Matching

We've already seen pattern matching a few times.
It is an incredibly versatile feature of Haskell, we can use it to do two main things:

1. Deconstruct complex values
2. Control flow

As we've seen when discussing
[newtypes](../03-html/04-safer_construction.html#using-newtypes),
we can use **case expressions** and **function definitions** to deconstruct a `newtype`,
but we can do that for `data` types as well:

```hs
-- | A data type representing colors
data Color
  = RGB Word8 Word8 Word8

getBluePart :: Color -> Word8
getBluePart color =
  case color of
    RGB _ _ blue -> blue
```

In `getBluePart` we deconstruct a composite value into its part and extract the third component
representing the blue value in a color represented by red, green and blue components (RGB).

Note that `blue` is the name we give to the third component so it will be bound
to the right of the arrow that comes after the pattern. This is similar to
a function argument. Also note that `_` matches any value *without* binding it to a name.

We can also try to match a value with more than one pattern:

```hs
data Brightness
  = Dark
  | Bright

data EightColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

data AnsiColor
  = AnsiColor Brightness EightColor

ansiColorToVGA :: AnsiColor -> Color
ansiColorToVGA ansicolor =
  case ansicolor of
    AnsiColor Dark Black ->
      RGB 0 0 0
    AnsiColor Bright Black ->
      RGB 85 85 85
    AnsiColor Dark Red ->
      RGB 170 0 0
    AnsiColor Bright Red ->
      RGB 255 85 85
    -- and so on
```

It's important to notice a few things here:

1. Patterns can be nested, notice how we deconstructed `ansicolor` on multiple levels
2. We try to match patterns from the top down, it is possible for patterns to overlap with one another and the top one will win
3. If the value we try to match does not match any of the patterns listed, an error will be thrown at runtime

We can ask GHC to notify us when we accidentally write overlapping patterns,
or when we haven't listed enough patterns to match all possible values,
by passing the flag `-Wall` to `ghc` or `runghc`.

**My recommendation is to always use `-Wall`**!

> As an aside, while it is possible to use pattern matching in function definitions by defining a function
> multiple times, [I personally don't like that feature very much](https://twitter.com/_gilmi/status/1257225601079029760)
> and I would encourage you to avoid it,
> but if you want to use it instead of case expressions, it is possible.

---

Exercises:

1. Create a function `isBright :: AnsiColor -> Bool` that checks whether a color is bright or not
2. Use [this table](https://en.wikipedia.org/wiki/ANSI_escape_code#3-bit_and_4-bit) to write `ansiToUbuntu`.
3. Create a function `isEmpty :: [a] -> Bool` that uses `listToMaybe` to check whether a list is empty or not
3. Create a function `isEmpty :: [a] -> Bool` that *doesn't* use `listToMaybe` to check whether a list is empty or not

---

## Parsing with rich context

Previously we wrote a parser that separates documents into different paragraphs.
With new features under our belt we can now remember the exact context we are in
(whether it is a text paragraph, a list, or a code block) and act accordingly!

Let's look again at the parsing code we wrote previously:

```hs
parse :: String -> Document
parse = parseLines [] . lines -- (1)

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
  let
    paragraph = Paragraph (unlines (reverse currentParagraph)) -- (2), (3)
  in
    case txts of
      [] -> [paragraph]
      currentLine : rest ->
        if trim currentLine == ""
          then
            paragraph : parseLines [] rest -- (4)
          else
            parseLines (currentLine : currentParagraph) rest -- (5)

trim :: String -> String
trim = unwords . words
```

Previously our context, `currentParagraph`, was used to group adjacent lines in an accumulative list.

Next, instead of using a `[String]` type to denote adjacent lines, we can instead use a `Structure` to denote the context.

One issue we might have though with representing context with the `Structure` type,
is that when we start parsing we don't have any context.
But we learned of a way to represent the possibility of an absence of a value with `Maybe`! So our new context type can be `Maybe Structure` instead.

Let's rewrite our code above to use our new context type:

```hs
parse :: String -> Document
parse = parseLines Nothing . lines -- (1)

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    [] -> maybeToList context   -- (2)
    -- Paragraph case
    currentLine : rest ->
      let
        line = trim currentLine
      in
        if line == ""
          then
            maybe id (:) context (parseLines Nothing rest) -- (3)
          else
            case context of
              Just (Paragraph paragraph) ->
                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest -- (4), (5)
              _ ->
                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words
```


1. We can now pass `Nothing` when we don't have a context
2. Unsure what `maybeToList` does? [Hoogle](https://hoogle.haskell.org) it!
3. [maybe](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:maybe) is a function that works similarly to pattern matching on a `Maybe`.
   Check out the types of `id`, `(:)` and `maybe id (:)` in GHCi!
4. Hey! Didn't we say that appending `String`s/lists is slow (which is what `unwords` does)? Yes, it is.
   Because in our `Structure` data type, a paragraph is defined as `Paragraph String` and not `Paragraph [String]`,
   we can't use our trick of building a list of lines and the reverse it at the end.

   So what do we do?
   There are many ways to handle that, one simple way is to create a different type with the right shape:

   ```hs
   data Context
     = CtxHeader Natural String
     | CtxParagraph [String]
     | CtxUnorderedList [String]
     | CtxOrderedList [String]
     | CtxCodeBlock [String]
   ```

   Since creating new types in Haskell is cheap, this is a very viable solution.

   In this case I'm going with the approach of not worrying about it too much,
   because it's a very local piece of code that can easily be fixed later if we see that it's an issue.

5. Anyway, if you've used `-Wall` like I've suggested,
   you'd get a warning from GHC saying that the *"pattern matches are non-exhaustive"*.
   This is because we did not cover all cases. So let's cover more cases:


```hs
parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    -- done case
    [] -> maybeToList context
   
    -- Header 1 case
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Header 1 (trim line) : parseLines Nothing rest)
   
    -- Unordered list case
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          parseLines (Just (UnorderedList (list <> [trim line]))) rest

        _ ->
          maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)
   
    -- Paragraph case
    currentLine : rest ->
      let
        line = trim currentLine
      in
        if line == ""
          then
            maybe id (:) context (parseLines Nothing rest)
          else
            case context of
              Just (Paragraph paragraph) ->
                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
              _ ->
                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words
```

---

Exercise: Add the `CodeBlock` and `OrderedList` cases.

<details>
  <summary>Final module</summary>

```hs
-- Markup.hs

module Markup
  ( Document
  , Structure(..)
  , parse
  )
where

import Numeric.Natural
import Data.Maybe (maybeToList)

type Document
  = [Structure]

data Structure
  = Header Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Eq, Show)    -- (1)


parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    -- done case
    [] -> maybeToList context
   
    -- Header 1 case
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Header 1 (trim line) : parseLines Nothing rest)
   
    -- Unordered list case
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          parseLines (Just (UnorderedList (list <> [trim line]))) rest

        _ ->
          maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)

    -- Ordered list case
    ('#' : ' ' : line) : rest ->
      case context of
        Just (OrderedList list) ->
          parseLines (Just (OrderedList (list <> [trim line]))) rest

        _ ->
          maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)

    -- Code block case
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock code) ->
          parseLines (Just (CodeBlock (code <> [line]))) rest

        _ ->
          maybe id (:) context (parseLines (Just (CodeBlock [line])) rest)

    -- Paragraph case
    currentLine : rest ->
      let
        line = trim currentLine
      in
        if line == ""
          then
            maybe id (:) context (parseLines Nothing rest)
          else
            case context of
              Just (Paragraph paragraph) ->
                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest
              _ ->
                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words
```
</details>

---

### How do we know our parser works correctly?

In an earlier chapter, we parsed a few examples of our markup language [by hand](01-data_type.html#exercises).
Now, we can try to test our parser by comparing our solutions to our parser.
By adding the `Eq` constraint to our data type (as shown in (1)), we can add these to our module and
use the `==` (equals) operator to compare our solutions to the result our parser gives.

Try it in GHCi! You can read a text file in GHCi using the following syntax:

```hs
ghci> txt <- readFile "/tmp/sample.txt"
```

And then compare with the hand written example values from the solutions
(after adding them to the module and loading them in GHCi):

```hs
ghci> parse txt == example4
```

In a later chapter, we'll discuss how to use a testing framework and
write automated tests for our parser. But first, I'd like to glue things together
so we'll be able to:

1. Read markup text from a file
2. Parse the text
3. Convert the result to our HTML EDSL
4. Generate HTML code

And also discuss how to work with IO in Haskell while we're at it.
