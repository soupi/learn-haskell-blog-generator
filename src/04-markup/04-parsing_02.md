# Parsing markup part 02 (Pattern matching)

## Maybe

Previously on partial functions, we mentioned that one way to avoid
writing partial functions is to encode the absence of a result using `Maybe`:

```hs
data Maybe a
  = Nothing
  | Just a
```

`Maybe` is a data type from the standard library (named [base](https://hackage.haskell.org/package/base))
for adding an additional value to a type: the absence of a value.
For example, `Maybe Bool` has three values,
two with the `Just` constructor to represent regular boolean values
(`Just True` and `Just False`) and another value, `Nothing` to represent
the absence of a boolean value.

We can use this to encode the result of `head`, a function that promises to return
the first element of a list, without creating a partial function:

```hs
safeHead :: [a] -> Maybe a
```

This way, when the list is empty, we can return `Nothing`, and when it has at least
one element, we can return `Just <first element>`. This function can be found in
the [Data.Maybe](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Maybe.html)
module under the name
[listToMaybe](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Maybe.html#v:listToMaybe).

In order to *consume* values of type `Maybe <something>`, and other types created with
`data`, we can use pattern matching.

## Pattern Matching

We've already seen pattern matching a few times.
It is an incredibly versatile feature of Haskell, we can use it to do two main things:

1. Deconstruct complex values
2. Control flow

As we've seen when discussing
[newtypes](../03-html/04-safer_construction.html#using-newtypes),
we can use **case expressions** and **function definitions** to deconstruct a `newtype`.
Same for `data` types as well:

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

### Pattern matching on linked lists

Because linked lists have their own [special syntax](/03-html/06-escaping_characters.html#linked-lists-briefly),
we also have special syntax for their pattern match.
We can use the same special syntax for creating lists when we pattern match on lists,
replacing the *elements* of the list with patterns. For example:

```hs
safeHead :: [a] -> Maybe a
safeHead list =
  case list of
    -- Empty list
    [] -> Nothing

    -- Cons cell pattern, will match any list with at least one element
	x : _ -> Just x
```

```hs
exactlyTwo :: [a] -> Maybe (a, a)
exactlyTwo list =
  case list of
    -- Will match a list with exactly two elements
	[x, y] -> Just (x, y)

    -- Will match any other pattern
	_ -> Nothing
```

```hs
-- This will also work
exactlyTwoVersion2 :: [a] -> Maybe (a, a)
exactlyTwoVersion2 list =
  case list of
    -- Will match a list with exactly two elements
	x : y : [] -> Just (x, y)

    -- Will match any other pattern
	_ -> Nothing
```


---

Exercises:

1. Create a function `isBright :: AnsiColor -> Bool` that checks whether a color is bright
2. Use [this table](https://en.wikipedia.org/wiki/ANSI_escape_code#3-bit_and_4-bit) to write `ansiToUbuntu`
3. Create a function `isEmpty :: [a] -> Bool` that uses `listToMaybe` to check whether a list is empty
4. Create a function `isEmpty :: [a] -> Bool` that *doesn't* use `listToMaybe` to check whether a list is empty

Solutions:

<details><summary>Solution for (1)</summary>

```hs
isBright :: AnsiColor -> Bool
isBright ansiColor =
  case ansiColor of
    AnsiColor Bright _ -> True
    AnsiColor Dark _ -> False
```

</details>
<details><summary>Solution for (2)</summary>

```hs
ansiToUbuntu :: AnsiColor -> Color
ansiToUbuntu ansiColor =
  case ansiColor of
    AnsiColor brightness color ->
      case brightness of
        Dark ->
          case color of
            Black -> RGB 0 0 0
            Red -> RGB 194 54 33
            Green -> RGB 37 188 36
            Yellow -> RGB 173 173 39
            Blue -> RGB 73 46 225
            Magenta -> RGB 211 56 211
            Cyan -> RGB 51 187 200
            White -> RGB 203 204 205

        Bright ->
          case color of
            Black -> RGB 129 131 131
            Red -> RGB 252 57 31
            Green -> RGB 49 231 34
            Yellow -> RGB 234 236 35
            Blue -> RGB 88 51 255
            Magenta -> RGB 249 53 248
            Cyan -> RGB 20 240 240
            White -> RGB 233 235 235
```

Since pattern matching goes arbitrarily deep as we saw before, we could instead
pattern match all the way through in one case expression:

```hs
ansiToUbuntu :: AnsiColor -> Color
ansiToUbuntu ansiColor =
  case ansiColor of
    AnsiColor Dark Black -> RGB 0 0 0
    AnsiColor Dark Red -> RGB 194 54 33
    AnsiColor Dark Green -> RGB 37 188 36
    AnsiColor Dark Yellow -> RGB 173 173 39
    AnsiColor Dark Blue -> RGB 73 46 225
    AnsiColor Dark Magenta -> RGB 211 56 211
    AnsiColor Dark Cyan -> RGB 51 187 200
    AnsiColor Dark White -> RGB 203 204 205
    AnsiColor Bright Black -> RGB 129 131 131
    AnsiColor Bright Red -> RGB 252 57 31
    AnsiColor Bright Green -> RGB 49 231 34
    AnsiColor Bright Yellow -> RGB 234 236 35
    AnsiColor Bright Blue -> RGB 88 51 255
    AnsiColor Bright Magenta -> RGB 249 53 248
    AnsiColor Bright Cyan -> RGB 20 240 240
    AnsiColor Bright White -> RGB 233 235 235
```

But this is a bit too much repetition of `AnsiColor`, `Dark` and `Bright`
to my taste in this case.

</details>
<details><summary>Solution for (3)</summary>

```hs
isEmpty :: [a] -> Bool
isEmpty list =
  case listToMaybe list of
    Nothing -> True
    Just _ -> False
```

</details>
<details><summary>Solution for (4)</summary>

```hs
isEmpty :: [a] -> Bool
isEmpty list =
  case list of
    [] -> True
    _ : _ -> False
```

</details>




---

## Parsing with rich context

Previously we wrote a parser that separates documents into different paragraphs.
With new features under our belt we can now remember the exact context we are in
(whether it is a text paragraph, a list, or a code block) and act accordingly!

Let's look again at the parsing code we wrote previously:

```hs
parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
  let
    paragraph = Paragraph (unlines (reverse currentParagraph))
  in
    case txts of
      [] -> [paragraph]
      currentLine : rest ->
        if trim currentLine == ""
          then
            paragraph : parseLines [] rest
          else
            parseLines (currentLine : currentParagraph) rest

trim :: String -> String
trim = unwords . words
```

Previously our context, `currentParagraph`, was used to group adjacent lines in an accumulative list.

Next, instead of using a `[String]` type to denote adjacent lines, we can instead use a `Structure` to denote the context.

One issue we might have though with representing context with the `Structure` type,
is that when we start parsing we don't have any context.
But we have learned of a way to represent the absence of a value with `Maybe`! So our new context type can be `Maybe Structure` instead.

Let's rewrite our code above with our new context type:

```hs
parse :: String -> Document
parse = parseLines Nothing . lines -- (1)

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    [] -> maybeToList context -- (2)
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
                parseLines (Just (Paragraph (unwords [paragraph, line]))) rest -- (4)
              _ ->
                maybe id (:) context (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words
```


1. We can now pass `Nothing` when we don't have a context
2. Unsure what `maybeToList` does? [Hoogle](https://hoogle.haskell.org) it!
3. We can split this line into two important parts:
   1. `maybe id (:) context` - prepending the context to the rest of the document
   2. `parseLines Nothing rest` - parsing the rest of the document

   Let's focus on the first part.
   We want to prepend `context` to the rest of the document, but we can't write
   `context : parseLines Nothing rest` because `context` has the type `Maybe Structure`
   and not `Structure`, meaning that we *might* have a `Structure` but maybe not.
   If we do have a `Structure` to prepend, we wish to prepend it. If not, we want to return
   the result of `parseLines Nothing rest` as is. Try writing this using pattern matching!

   <details><summary>Solution</summary>

   ```hs
   case context of
     Nothing -> parseLines Nothing rest
     Just structure -> structure : parseLines Nothing rest
   ```

   </details>

   The [maybe](https://hackage.haskell.org/package/base-4.16.4.0/docs/Prelude.html#v:maybe)
   function let's us do the same thing in a more compact way. It is a function
   that works similarly to pattern matching on a `Maybe`:
   the third argument to `maybe` is the value on which we pattern match,
   the second argument is a function to apply to the value found in a `Just` case,
   and the first argument is the value to return in case the value
   we pattern match on is `Nothing`. A more faithful translation of
   `maybe id (:) context (parseLines Nothing rest)`
   to pattern matching would look like this:

   <details><summary>Solution</summary>

   ```hs
   ( case context of
       Nothing -> id
       Just structure -> (:) structure
   ) (parseLines Nothing rest)
   ```

   Note how the result of this case expression is a function of type `Document -> Document`,
   how we partially apply `(:)` with `structure` to create a function that prepends `structure`,
   and how we apply `parseLines Nothing rest` to the case expression.

   </details>

   This way of encoding pattern matching using functions is fairly common.

   Check out the types of `id`, `(:)` and `maybe id (:)` in GHCi!

4. Hey! Didn't we say that appending `String`s/lists is slow (which is what `unwords` does)? Yes, it is.
   Because in our `Structure` data type, a paragraph is defined as `Paragraph String` and not `Paragraph [String]`,
   we can't use our trick of building a list of lines and then reverse it in the end.

   So what do we do?
   There are many ways to handle that, one simple way is to create a different type with the right shape:

   ```hs
   data Context
     = CtxHeading Natural String
     | CtxParagraph [String]
     | CtxUnorderedList [String]
     | CtxOrderedList [String]
     | CtxCodeBlock [String]
   ```

   Since creating new types in Haskell is cheap, this is a very viable solution.

   In this case I'm going with the approach of not worrying about it too much,
   because it's a very local piece of code that can easily be fixed later if needed.

Let's cover more parsing cases, we want to handle headings and lists as well.
We can do that by examining the first characters of a line:

```hs
parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    -- done case
    [] -> maybeToList context

    -- Heading 1 case
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)

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
  = Heading Natural String
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

    -- Heading 1 case
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)

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
By deriving `Eq` for our `Structure` data type
(marked with (1) in "final module" above),
we can compare solutions with the `==` (equals) operator.

Try it in GHCi! You can read a text file in GHCi using the following syntax:

```hs
ghci> txt <- readFile "/tmp/sample.txt"
```

And then compare with the hand written example values from the solutions
(after adding them to the module and loading them in GHCi):

```hs
ghci> parse txt == example4
```

In a later chapter, we'll write automated tests for our parser using a testing framework.
But before that, I'd like to glue things together
so we'll be able to:

1. Read markup text from a file
2. Parse the text
3. Convert the result to our HTML EDSL
4. Generate HTML code

And also discuss how to work with IO in Haskell while we're at it.

> You can view the git commit of
> [the changes we've made](https://github.com/soupi/learn-haskell-blog-generator/commit/9f951a05d4f78cf59190ee4f3cd8de85e1c33bd1)
> and the [code up until now](https://github.com/soupi/learn-haskell-blog-generator/tree/9f951a05d4f78cf59190ee4f3cd8de85e1c33bd1).
