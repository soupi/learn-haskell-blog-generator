# Safer HTML construction with types

In this section we'll learn how to create our own distinguished types
for HTML, and how they can help us avoid invalid construction of HTML strings.

There are a few ways of defining new types in Haskell, in this section
we are going to meet two ways: `newtype` and `type`.


## `newtype`

`newtype` lets us give a new name to an already existing type in a
way that the two cannot mix together.

A `newtype` declaration looks like this:

```hs
newtype <type-name> = <constructor> <existing-type>
```

For example in our case we can define a distinct type for `Html` like this:

```hs
newtype Html = Html String
```

The first `Html`, to the left of the equals sign, lives in the _types_
name space, meaning that you will only see that name to the right of a
double-colon sign (`::`).

The second `Html` lives in the _expressions_ (or terms/values) name space,
meaning that you will see it where you expect expressions (we'll touch where
exactly that can be in a moment).

The two names, `<type-name>` and `<constructor>`, do not have to be the
same, but they often are. And note that both have to start with a
capital letter.

The right-hand side of the newtype declaration describes how an
expression of that type looks like. In our case, we expect a value of
type `Html` to have the constructor `Html` and then an expression of
type string, for example: `Html "hello"` or `Html ("hello " <>
"world")`.

You can think of the constructor as a function that takes the argument
and returns something of our new type:

```hs
Html :: String -> Html
```

**Note**: We cannot use an expression of type `Html` the same way we'd
use a `String`. so `"hello " <> Html "world"` would fail at type
checking.

This is useful when we want *encapsulation*. We can define and use
existing representation and functions for our underlying type, but not
mix them with other, unrelated (to our domain) types. Similar as
meters and feet can both be numbers, but we don't want to accidentally
add feet to meters without any conversion.

To get this actually working well, we'll need a bit more than just
newtypes. In the next chapter we'll introduce modules and smart constructors.

---

For now, let's create a couple of types for our use case.
We want two separate types to represent:

1. A complete Html document
2. A type for nodes that can go inside the <body> tag

We want them to be distinct because we don't want to mix them together.

<details>
  <summary>Solution</summary>

```hs
newtype Html = Html String

newtype Structure = Structure String
```

</details>

---

## `type`

A `type` definition looks really similar - the only difference is that
we have no constructor:

```hs
type <type-name> = <existing-type>
```

For example in our case we can write:

```hs
type Title = String
```

`type`, on the other hand, is just a name alias. so `Title`
and `String` are interchangeable. We can use `type`s
to give a bit more clarity to our code.

## Using `newtype`s

Back to `newtype`s. So how can we use the underlying type? We first
need to extract it out of the type. We do this using pattern matching.

Pattern matching can be used in two ways, in case expressions and in
function definitions.

1. case expressions are kind of beefed up switch expressions and look like this:

```hs
case <expression> of
  <pattern> -> <expression>
  ...
  <pattern> -> <expression>
```

The `<expression>` is the thing we want to unpack, and the `pattern`
is its concrete shape. For example:

```hs
getStructureString :: Structure -> String
getStructureString struct =
  case struct of
    Structure str -> str
```

This way we can extract the String out of `Structure` and return
it.

In later commits we'll introduce `data` declarations (which are kind of
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
getStructureString :: Structure -> String
getStructureString (Structure str) = str
```

Using the types we created, we can change the HTML functions we've defined before,
namely `html_`, `body_`, `p_`, etc, to operate on these types instead of `String`s.

But first let's meet another operator that will make our code more concise.

## Chaining functions

Another interesting and extremely common operator
(which is a regular library function in Haskell) is `.` (pronounced compose).
This operator was made to look like the composition operator
you may know from math (`∘`).

Let's look at its type and implementation:

```hs
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)
```

Compose takes 3 arguments: two functions (named `f` and `g` here) and
a third argument named `x`. It then passes the argument `x` to the second
function `g`, and calls the first function `f` with the result of `g x`.

Note that `g` takes as input something of the type
`a` and returns something of the type `b`, and `f` takes
something of the type `b`, and returns something of the type `c`.

Another important thing to note is that types which start with
a _lowercase letter_ are **type variables**.
Think of them as similar to regular variables. Just like
`content` could be any string, like `"hello"` or `"world"`, a type variable
can be any type: `Bool`, `String`, `String -> String`, etc.
This abilitiy is called *parametric polymorphism* (other languages often call this generics).

The catch is that type variables must match in a signature, so if for
example we write a function with the type signature `a -> a`, the
input type and the return type **must** match, but it could be
any type - we cannot know what it is. So the only way to implement a
function with that signature is:

```hs
id :: a -> a
id x = x
```

`id`, short for the identity function, returns the exact value it received.
If we tried any other way, for example returning some made up value
like `"hello"`, or try to use `x` like a value of a type we know like
writing `x + x`, the type checker will complain.

Also, remember that `->` is right associative? This signature is equivalent to:

```hs
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

Doesn't it look like a function that takes two functions and returns a
third function that is the composition of the two?

We can now use this operator to change our HTML functions. Let's start
with one example: `p_`.

Before, we had:

```hs
p_ :: String -> String
p_ = el "p"
```

And now, we can write:

```hs
p_ :: String -> Structure
p_ = Structure . el "p"
```

The function `p_` will take an arbitrary `String` which is the content
of the paragraph we wish to create, will wrap it in `<p>` tags,
and then wrap it in the `Structure` constructor - producing the
output type `Structure`.

Let's take a deeper look and see what are the types of the two
functions here are:

- `Structure :: String -> Structure`
- `el "p" :: String -> String`
- `Structure . el "p" :: String -> Structure`
- `(.) :: (b -> c) -> (a -> b) -> (a -> c)`

When we try to figure out if an expression type checks, we try to match
the types and see if they work. If they are the same type, all is
well. If one of them is a type variable and the other isn't we write
down that the type variable should now be the concrete type, and see
if everything still works.

So in our case we know from the type signature that the input type to
the function `String` and the output type is `Structure`, this
means:

1. `a` is equivalent to `String` (we write `~` to denote equivalence), and
2. `c ~ Structure`

We also know that:

3. `b ~ String` because we pass `Structure` to `.` as the first arguments, which means
4. `String -> Structure` must
match with the type of the first argument of `.` which is `b -> c`, so
5. `b ~ String` which fits with our previous knowledge from (3)
6. `-> ~ ->`
7. `c ~ Structure` which also fits with (2)

We keep doing this process until we come to the conclusion that there
aren't any types that don't match (we don't have two different
concrete types that are supposed to be equivalent).

> **Note**: If we use a *parametrically polymorphic* function more than once,
> or use different functions that have similar type variable names,
> the type variables don't have to match in all instances simply because the share a name.
> Each instance has its own unique set of type variables. For example:
> 
> ```hs
> id :: a -> a
> ord :: Char -> Int
> chr :: Int -> Char
> 
> incrementChar :: Char -> Char
> incrementChar c = chr (ord (id c) + id 1)
> ```
> 
> In the snippet above, we use `id` twice (for no good reason other than for demonstration purposes).
> The first `id` takes a `Char` as an argument, and its `a` is equivalent to `Char`.
> The second `id` takes an `Int` as an argument, and its *distinct* `a` is equivalent to `Int`.
> 
> This unfortunately only applies to functions defined at the top-level. If we'd define a local function
> to be passed as an argument to `incrementChar` with the same type signature as `id`,
> the types must match in all uses. So this code:
> 
> ```hs
> incrementChar :: (a -> a) -> Char -> Char
> incrementChar func c = chr (ord (func c) + func 1)
> ```
> 
> Will not type check.

## Appending Structure

Before when we wanted to create richer HTML content and appended
nodes to one another, we used the append (`<>`) operator.
Since we are now not using `String` anymore, we need another way
to do it.

While it is possible to overload `<>` using a feature in
Haskell called type classes, we will instead create a new function
and call it `append_`, and cover type classes later.

`append_` should take two `Structure`s, and return a third `Structure`,
appending the inner `String` in the first `Structure` to the second and wrapping the result back in `Structure`.

---

Try implementing `append_`.

<details>
  <summary>Solution</summary>

```hs
append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) =
  Structure (a <> b)
```

</details>

---

## Converting back an `Html` to `String`

After constructing a valid `Html` value, we want to be able to
print it to the output so we can display it in our browser.
For that, we need to write a function that takes an `Html` and converts it to a `String`, which we can then pass to `putStrLn`.

---

Implement the `render` function.

<details>
  <summary>Solution</summary>

```hs
render :: Html -> String
render html =
  case html of
    Html str -> str
```

</details>

---

## The rest of the owl

---

Try changing the code we wrote in previous chapters to use the new types we created.

**Tips**: we can combine `makeHtml` and `html_`, and remove `body_` `head_` and `title_`
by calling `el` directly in `html_`, which can now have the type `Title -> Structure -> Html`. This will make our HTML EDSL less flexible but more compact.

We could, alternatively, create newtypes for `HtmlHead` and `HtmlBody` and
pass those to `html_`, and there is value in doing that, but I've chose
to keep the API a bit simple for now, we can always refactor later!

<details>
  <summary>Solution</summary>

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

</details>

---

All of this is nice and fun. And indeed now we can't write `"Hello"`
where we'd expect either a paragraph or a header, but we can still
write `Structure "hello"` and get something that isn't a
paragraph or a header. Next we'll see how we can make this illegal as
well using modules and smart constructors.
