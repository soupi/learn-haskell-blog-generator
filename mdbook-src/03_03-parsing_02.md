# Parsing markup part 02 (Pattern matching)

## Maybe

```hs
data Maybe a
    = Nothing
    | Just a
```

The data type `Maybe` from the standard library is used to add an additional
value to a type: the absence of a value. For example, `Maybe True` has three values,
two with the `Just` constructor to represent regular boolean values
(`Just True` and `Just False`) and another value, `Nothing` to represent
the absence of a boolean value.

<!--

We can use this 

We can use pattern matching to process this information, matching on
the `Nothing` or `Just` cases, or we could use a function such as
`maybe`:

```
maybe :: b -> (a -> b) -> Maybe a -> b
maybe nothingCase func x =
  case x of
    Nothing -> nothingCase
    Just v -> func v
```

Maybe is useful in many many situations.

---

(3) Recursion and accumulating information

Instead of loops, in Haskell we use recursion to model iteration.

In our case, we define a local function inside `parse` named `impl`
and refer to it inside itself when we want to process the next line
using the information we learned from the current line.

In `impl`, we process a line using information from the current part
we are processing. For example, if the line we are currently trying to
parse is a regular paragraph line, we will want to check if the last
line was also part of a paragraph so if it was, we can group them
together.
If it wasn't we want to mark that part as done, return it, but wait
for the next lines to know if the current line that is part of a
paragraph could be grouped to the next line as well.

When we are done processing some markup part, we say that we return a
list where the first element of it is the markup part we parsed, and
the rest of it is the parsing of the next lines.

We are done recursing when there are no more lines to process.

Note that the order of pattern matching *matters*. Haskell will choose
the first pattern in order that matches.

Let's break down a part of the code:

```
('@' : ' ' : line) : rest ->
  maybe id (:) current (Header 1 line : impl Nothing rest)
```

The thing we are pattern matching on is the current line (and the rest
of the lines) and we want to see if this line is a header.
If it is, we do the following:

```
  maybe id (:) current (Header 1 line : impl Nothing rest)
```

Let's break it down further:

`Header 1 line : impl Nothing rest` - since our line is a header, we
will want to return a `Header`, and process the rest of the lines
without taking account previous lines (because headers do not group
with other lines)

`maybe id (:) current` - here we take care of the information from
previous rows. If, for example, we had a paragraph from the previous
line, `current` will be `Just (Paragraph ...)`, s we'd like to do
`(:) current (Header 1 line : impl Nothing rest)`.
If for example the previous line was an empty line or another header,
then there isn't any information that carried from previous lines and
`current` would be `Nothing, so in this case we don't want to add
anything and will want to have
`id (Header 1 line : impl Nothing rest)`.

`id` is the identity function. it looks like this:

```
id :: a -> a
id x = x
```

It returns what it gets. Does nothing.

---

This code is a bit more involved than what we saw previously, so don't
worry if it takes to time to figure out. Try to simulate how the code
behaves at each iteration with different inputs.

-->
