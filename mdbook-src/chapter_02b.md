# Adding type signatures

Haskell is a statically typed language. That means that every
expression has a type, and we check that the types are valid with
regards to each other before running the program. If we discover that
they are not valid, an error message will be printed and the program
will not run.

An example of type error would be if we'd pass a function more
arguments that it take, or pass a number instead of a string.

Haskell is also type inferred, so we don't *need* to specify the type
of expressions - Haskell can infer from the context of the expression
what its type is, and that's what we did up until now. But specifying
types is useful - it adds a layer of documentation for you or others
that will look at the code later, and it helps verify to some degree
that what was intended (with the type signature) is what was
written (with the expression).

We use double-colon (`::`) to specify the type of names. We usually
write it right above the definition of the name itself.

Here are a few examples of types we can write:

- `Int` - The type of integer numbers
- `String` - The type of strings
- `Bool` - The type of booleans
- `()` - The type of the expression `()`, also called unit
- `a -> b` - The type of a function from an expression of type `a` to an expression of type `b`
- `IO ()` - The type of an expression that represents an IO subroutine that returns `()`

Let's specify the type of `title_`:

```hs
title_ :: String -> String
```

We can see in the code that the type of `title_` is a function that takes
a `String` and returns a `String`.

Let's also specify the type of `makeHtml`:

```hs
makeHtml :: String -> String -> String
```

Previously, we thought about `makeHtml` as a function that takes
two strings and returns a string.

But actually, all functions in Haskell take **exactly one argument** as input
and return **exactly one value** as output. It's just convenient to refer
to functions like `makeHtml` as functions with multiple inputs.

In our case, `makeHtml` is a function that takes **one** string argument,
and returns a **function**. _The function it returns_ takes a string argument
as well and finally returns a string.

The magic here is that `->` is right associative.

When we write:

```hs
makeHtml :: String -> String -> String
```

Haskell parses it as:

```hs
makeHtml :: String -> (String -> String)
```

Consecuently, the expression `makeHtml "My title"` is also a function!
One that will take a string (the content) and will return the expected
html string with "My title" in the title.

This is called _partial application_.

To illustrate, let's define `html_` and `body_` in a different way by
defining a new function, `el`.

```hs
el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"
```

el is a function that takes a tag and content, and wraps the content
with the tag.

We can now implement `html_` and `body_` by partially applying `el` and
only provide the tag.

```hs
html_ :: String -> String
html_ = el "html"

body_ :: String -> String
body_ = el "body"
```

Note that we didn't need to add the argument on the left side of
equals sign because Haskell functions are "first class" - they behave
exactly like normal expressions. You can define names to them like
regular values, put them in data structures, pass them to functions,
everything you can do with regular values like `Int` or `String`.

The way Haskell treats names is very similar to copy paste. anywhere
you see `html_` in the code, you can replace it with `el "html"`. They are
the same (this is what the equals signs say, right? That the two sides
are the same).

This property, of behind able to substitute the two sides of the
equals sign with one another, is called referential transparency. And
it is pretty unique to Haskell (and a few language that are very
similar to it like PureScript and Elm)!

---

Exercises:

1. Add types for all of the functions we created until now

2. Change the implementation of the html functions we built to use `el` instead

3. Add a couple more functions for defining paragraphs and headers:
   1. `p_` which uses the tag `<p>` for paragraphs
   2. `h1_` which uses the tag `<h1>` for headers

---

Solutions:

<details>
  <summary>Solution for exercise #1</summary>
  
  ```hs
  myhtml :: String
  myhtml = makeHtml "Hello title" "Hello, world!"

  makeHtml :: String -> String -> String
  makeHtml title content = html_ (head_ (title_ title) <> body_ content)

  html_ :: String -> String
  html_ content = "<html>" <> content <> "</html>"
     
  body_ :: String -> String
  body_ content = "<body>" <> content <> "</body>"

  head_ :: String -> String
  head_ content = "<head>" <> content <> "</head>"

  title_ :: String -> String
  title_ content = "<title>" <> content <> "</title>"
  ```

</details>

<details>
  <summary>Solution for exercise #2</summary>
  
  ```hs
  html_ :: String -> String
  html_ = el "html"
     
  body_ :: String -> String
  body_ = el "body"

  head_ :: String -> String
  head_ = el "head"

  title_ :: String -> String
  title_ = el "title"
  ```

</details>


<details>
  <summary>Solution for exercise #3</summary>
  
  ```hs
  p_ :: String -> String
  p_ = el "p"

  h1_ :: String -> String
  h1_ = el "h1"
  ```

</details>


---

<details>
  <summary>Our final program</summary>
  
  ```hs
  -- hello.hs

  main :: IO ()
  main = putStrLn myhtml

  myhtml :: String
  myhtml = makeHtml "Hello title" "Hello, world!"

  makeHtml :: String -> String -> String
  makeHtml title content = html_ (head_ (title_ title) <> body_ content)

  html_ :: String -> String
  html_ = el "html"
     
  body_ :: String -> String
  body_ = el "body"

  head_ :: String -> String
  head_ = el "head"

  title_ :: String -> String
  title_ = el "title"

  p_ :: String -> String
  p_ = el "p"

  h1_ :: String -> String
  h1_ = el "h1"

  el :: String -> String -> String
  el tag content =
    "<" <> tag <> ">" <> content <> "</" <> tag <> ">"
  ```

</details>
