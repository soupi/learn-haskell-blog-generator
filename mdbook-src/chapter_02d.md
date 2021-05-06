# Safer HTML library with types

There are a few ways of defining new types in Haskell, in this section
we are going to meet two ways: `newtype` and `type`.

1. `newtype` lets us give a new name to an already existing type in a
way that the two cannot mix together.

A `newtype` declaration looks like this:

```hs
newtype <type-name> = <constructor> <existing-type>
```

For example in our case we have:

```hs
newtype Html = Html String
```

The first `Html`, to the left of the equals sign, lives in the _types_
namespace, meaning that you will only see that name to the right of a
double-colon sign (`::`).

The second `Html` lives in the _expressions_ namespace, meaning that
you will see it where you expect expressions (we'll touch where
exactly that can be in a moment).

The two names, `<type-name>` and `<constructor>`, do not have to be the
same, but they often are. And note that both have to start with a
capital letter.

The right-hand side of the newtype declaration describes how an
expression of that type looks like. In our case, we expect a value of
type `Html` to have the constructor `Html` and then an expression of
type string, for example `Html "hello"` or `Html ("hello " <>
"world")`.

You can think of the constructor as a function that takes the argument
and returns something of our new type.

Note that we cannot use an expression of type `Html` the same way we'd
use a `String`. so `"hello " <> Html "world"` would fail at type
checking.

This is useful when we want *encapsulation*. We can define use
existing representation and functions for our underlying type, but not
mix them with other, unrelated (to our domain) types. Similar as
meters and feet can both be numbers, but we don't want to accidently
add feets to meters without any conversion.

To get this actually working well we'll need a bit more than just
newtypes, in the next commit we'll introduce modules and smart constructors.

2. A `type` definition looks really similar - the only difference is that
we have no constructor:

```hs
type <type-name> = <existing-type>
```

For example in our case we have:

```hs
type HtmlTitle = String
```

`type`, on the other hand, is just a name alias. so writing `HtmlTitle`
or `String` is exactly the same for Haskell, and we can use
it to give a bit more clarity to our code.

Back to `newtype`s. So how can we use the underlying type? We first
need to extract it out of the type. We do this using pattern matching.

Pattern matching can be used in two ways, in case expressions and in
function definitions.

1. case expressions are kinda beefed up switch expressions and look like this:

```hs
case <expression> of
  <pattern> -> <expression>
  ...
  <pattern> -> <expression>
```

The `<expression>` is the thing we want to unpack, and the `pattern`
is how it actually looks like. For example:

```hs
getBodyContentString :: HtmlBodyContent -> String
getBodyContentString myhbc =
  case myhbc of
    HtmlBodyContent str -> str
```

This way we can extract the String out of `HtmlBodyContent` and return
it.

In later commits we'll introduce `data` declarations (which are kinda
a struct + enum chimera), where we can define multiple constructors to
a type. Then the multiple patterns of a case expression will make more
sense.

2. Alternatively, when declaring a function, we can also use pattern matching on the
arguments:

```hs
func <pattern> = <expression>
```

For example:

```hs
getBodyContentString :: HtmlBodyContent -> String
getBodyContentString (HtmlBodyContent str) = str
```

And this is the way we use it in this code, because here it's a bit
more concise.

Another interesting operator (which is a regular library function in
Haskell) we are introducing here is `.`. Pronounced compose. This is
similar to the composition operator you may know from math. It takes
two functions and an argument, and passes the argument to the second
function, and the result of that is then passed to the first function.

The type for `.` is:

```hs
(.) :: (b -> c) -> (a -> b) -> a -> c
```

Note that the second function takes as input something of the type
`a`, returns something of the type `b`, and the first functions takes
something of the type `b`, and returns something of the type `c`.

Note that types that start with a lowercase letter are "type
variables". Think of them as similar to regular variables. Just like
`str` could be any string, like "hello" or "world", a type variable
can be any type: `Bool`, `String`, `String -> String`, etc.

The catch is that type variables must match in a signature, so if for
example we write a function with the type signature `a -> a`, the
types argument type and the return type *must* match. And it could be
any type - we cannot know what it is. So the only way to implement a
function with that signature is:

```hs
mysteryFunction :: a -> a
mysteryFunction x = x
```

If we tried any other way, for example returning some made up value
like "hello", or try to use `x` like a value of a type we know like
writing `x + x`, the type checker will complain.

Also, remember that `->` is right associative? So this signature is the same as:

```hs
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

Doesn't it look like a function that takes two functions and returns a
third function that is the composition of the two?

In our concrete example we have:

```hs
p_ :: String -> HtmlBodyContent
p_ = HtmlBodyContent . el "p"
```

Let's take a deeper look and see what are the types of the two
functions here are:

- `HtmlBodyContent :: String -> HtmlBodyContent`
- `el "p" :: String -> String`
- `HtmlBodyContent . el "p" :: String -> HtmlBodyContent`
- `(.) :: (b -> c) -> (a -> b) -> (a -> c)`

When we try to figure out if an expression type check, we try to match
the types and see if they work. If they are the same type, all is
well. If one of them is a type variable and the other isn't we write
down that the type variable should now be the concrete type, and see
if everything still works.

So in our case we know from the type signature that the input type to
the function `String` and the output type is `HtmlBodyContent`, this
means `a` is equivalent to `String` (we write `~` to denote
equivalence) and `c ~ HtmlBodyContent`. We also know that `b ~ String`
because we pass `HtmlBodyContent` to `.` as the first arguments, which
means `String -> HtmlBodyContent` (`HtmlBodyContent`'s type) must
match with the type of the first argument of `.` which is `b -> c`.

We keep doing this process until we come to the conclusion that there
aren't any types that don't match (we don't have two different
concrete types that are supposed to be equivalent).

All of this is nice and fun. And indeed now we can't write "Hello"
where we'd expect either a paragraph or a header, but we can still
write `HtmlBodyContent "hello"` and get something that isn't a
paragraph or a header. Next we'll see how we can make this illegal as
well.

---

```hs
-- hello.hs

main :: IO ()
main = putStrLn (render myhtml)

myhtml :: Html
myhtml =
  html_
    "My title"
    ( append_
      (h1_ "Header")
      ( append_
        (p_ "Paragraph #1")
        (p_ "Paragraph #2")
      )
    )

newtype Html
  = Html String

type HtmlTitle
  = String

newtype HtmlBodyContent
  = HtmlBodyContent String

html_ :: HtmlTitle -> HtmlBodyContent -> Html
html_ title content =
  Html
    ( el "html"
      ( el "head" (el "title" title)
        <> el "body" (getBodyContentString content)
      )
    )

p_ :: String -> HtmlBodyContent
p_ = HtmlBodyContent . el "p"

h1_ :: String -> HtmlBodyContent
h1_ = HtmlBodyContent . el "h1"

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

append_ :: HtmlBodyContent -> HtmlBodyContent -> HtmlBodyContent
append_ c1 c2 =
  HtmlBodyContent (getBodyContentString c1 <> getBodyContentString c2)

getBodyContentString :: HtmlBodyContent -> String
getBodyContentString content =
  case content of
    HtmlBodyContent str -> str

render :: Html -> String
render html =
  case html of
    Html str -> str
```
---
