# Adding type signatures

Haskell is a **statically typed** programming language. That means that every
expression has a type, and we check that the types are valid with
regard to each other before running the program. If we discover that
they are not valid, an error message will be printed, and the program
will not run.

An example of a type error would be if we'd pass 3 arguments to a function
that takes only 2, or pass a number instead of a string.

Haskell is also **type inferred**, so we don't *need* to specify the type
of expressions - Haskell can *infer* from the context of the expression
what its type should be, and that's what we have done until now. However, **specifying
types is useful** - it adds a layer of documentation for you or others
that will look at the code later, and it helps verify to some degree
that what was intended (with the type signature) is what was
written (with the expression). It is generally recommended to annotate all *top-level*
definitions with type signatures.

We use a double-colon (`::`) to specify the type of names. We usually
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

In our case, `makeHtml` is a function that takes **one** string argument
and returns a **function**. _The function it returns_ takes a string argument
as well and finally returns a string.

The magic here is that `->` is right-associative. This means that when we write:

```hs
makeHtml :: String -> String -> String
```

Haskell parses it as:

```hs
makeHtml :: String -> (String -> String)
```

Consequently, the expression `makeHtml "My title"` is also a function!
One that takes a string (the content, the second argument of `makeHtml`)
and returns the expected HTML string with "My title" in the title.

This is called **partial application**.

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
equals sign because Haskell functions are "**first class**" - they behave
exactly like values of primitive types like `Int` or `String`.
We can name a function like any other value,
put it in data structures, pass it to functions, and so on!

The way Haskell treats names is very similar to copy-paste. Anywhere
you see `html_` in the code, you can replace it with `el "html"`. They are
the same (this is what the equals signs say, right? That the two sides
are the same). This property of being able to *substitute* the two sides of the
equals sign with one another is called **referential transparency**. And
it is pretty unique to Haskell (and a few similar languages such as PureScript and Elm)!
We'll talk more about referential transparency in a later chapter.

### Anonymous/lambda functions

To further drive the point that Haskell functions are first class and
all functions take exactly one argument,
I'll mention that the syntax we've been using up until
now to define function is just syntactic sugar! We can also define
**anonymous functions** - functions without a name, anywhere we'd like.
Anonymous functions are also known as **lambda functions**
as a tribute to the formal mathematical system
which is at the heart of all functional programming
languages - the lambda calculus.

We can create an anonymous function anywhere we'd expect an expression,
such as `"hello"`, using the following syntax:

```hs
\<argument> -> <expression>
```

This little `\` (which bears some resemblance to the lowercase Greek letter lambda 'λ')
marks the head of the lambda function,
and the arrow (`->`) marks the beginning of the function's body.
We can even chain lambda functions, making them "multiple argument functions" by
defining another lambda in the body of another, like this:

```hs
three = (\num1 -> \num2 -> num1 + num2) 1 2
```

As before, we evaluate functions by substituting the function argument with
the applied value. In the example above, we substitute `num1` with `1` and get
`(\num2 -> 1 + num2) 2`. Then substitute `num2` with `2` and get `1 + 2`.
We'll talk more about substitution later.

So, when we write:

```hs
el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"
```

Haskell actually translates this under the hood to:

```hs
el :: String -> (String -> String)
el = \tag -> \content ->
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"
```

Hopefully, this form makes it a bit clearer why Haskell functions
always take one argument, even when we have syntactic sugar that
might suggest otherwise.

I'll mention one more syntactic sugar for anonymous functions:
We don't actually have to write multiple argument anonymous functions
this way, we can write:

```hs
\<arg1> <arg2> ... <argN> -> <expression>
```

to save us some trouble. For example:

```hs
three = (\num1 num2 -> num1 + num2) 1 2
```

But it's worth remembering what they are under the hood.

We won't be needing anonymous/lambda functions at this point,
but we'll discuss them later and see where they can be useful.

---

Exercises:

1. Add types for all of the functions we created until now

2. Change the implementation of the HTML functions we built to use `el` instead

3. Add a couple more functions for defining paragraphs and headings:
   1. `p_` which uses the tag `<p>` for paragraphs
   2. `h1_` which uses the tag `<h1>` for headings

4. Replace our `Hello, world!` string with richer content, use `h1_` and `p_`.
   We can append HTML strings created by `h1_` and `p_` using the append operator `<>`.

Bonus: rewrite a couple of functions using lambda functions, just for fun!

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

<details>
  <summary>Solution for exercise #4</summary>

  ```hs
  myhtml :: String
  myhtml =
    makeHtml
      "Hello title"
      (h1_ "Hello, world!" <> p_ "Let's learn about Haskell!")
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
  myhtml =
    makeHtml
      "Hello title"
      (h1_ "Hello, world!" <> p_ "Let's learn about Haskell!")


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
