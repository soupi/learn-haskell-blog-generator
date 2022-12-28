# Safer HTML construction with types

In this section we'll learn how to create our own distinguished types
for HTML, and how they can help us avoid invalid construction of HTML strings.

There are a few ways of defining new types in Haskell, in this section
we are going to meet two ways: `newtype` and `type`.

## `newtype`

A `newtype` declaration is a way to define a new, distinct type for an existing set of values.
This is useful when we want to reuse existing values but give them different meaning,
and make sure we can't mix the two.
For example, we can represent seconds, minutes, grams and yens using integer values,
but we don't want to accidentally mix grams and seconds.

In our case we want to represent structured HTML using textual values,
but distinguish them from everyday strings that are not valid HTML.

A `newtype` declaration looks like this:

```
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

The right-hand side of the newtype declaration describes the shape of a
value of that type. In our case, we expect a value of
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

---

For now, let's create a couple of types for our use case.
We want two separate types to represent:

1. A complete Html document
2. A type for html structures such as headings and paragraphs that can go inside the <body> tag

We want them to be distinct because we don't want to mix them together.

<details>
  <summary>Solution</summary>

```hs
newtype Html = Html String

newtype Structure = Structure String
```

</details>

---

## Using `newtype`s

In order to use the underlying type that the newtype wraps, we first
need to extract it out of the type. We do this using pattern matching.

Pattern matching can be used in two ways, in case expressions and in
function definitions.

1. case expressions are kind of beefed up switch expressions and look like this:

   ```
   case <expression> of
     <pattern> -> <expression>
     ...
     <pattern> -> <expression>
   ```

   The `<expression>` is the thing we want to unpack, and the `pattern`
   is its concrete shape. For example, if we wanted to extract the `String`
   out of the type `Structure` we defined in the exercise above, we do:

   ```hs
   getStructureString :: Structure -> String
   getStructureString struct =
     case struct of
       Structure str -> str
   ```

   This way we can extract the `String` out of `Structure` and return
   it.

   > In later chapters we'll introduce `data` declarations (which are kind of
   > a struct + enum chimera), where we can define multiple constructors to
   > a type. Then the multiple patterns of a case expression will make more
   > sense.

2. Alternatively, when declaring a function, we can also use pattern matching on the
arguments:

   ```
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

One very cool thing about `newtype` is that wrapping and extracting expressions doesn't actually
have a performance cost! The compiler knows how to remove any wrapping and extraction
of the `newtype` constructor and use the underlying type.

The new type and the constructor we defined are only there to help us *distinguish* between
the type we created and the underlying type when *we write our code*, they are not
needed *when the code is running*.

`newtype`s provide us with type safety with no performance penalty!

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
of the paragraph we wish to create, wrap it in `<p>` and `</p>` tags,
and then wrap it in the `Structure` constructor to produce the
output type `Structure` (remember: newtype constructors can be used as functions!).

Let's take a deeper look at the types:

- `Structure :: String -> Structure`
- `el "p" :: String -> String`
- `(.) :: (b -> c) -> (a -> b) -> (a -> c)`
- `Structure . el "p" :: String -> Structure`

Let's see why the expression `Structure . el "p"` type checks,
and why its type is `String -> Structure`.

### Type checking with pen and paper

If we want to figure out if and how exactly an expression type-checks,
we can do that rather systematically. Let's look at an example
where we try and type-check this expression:

```hs
p_ = Structure . el "p"
```

First, we write down the type of the outer-most function. In
our case this is the operator `.` which has the type:

```hs
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

After that, we can try to **match** the type of the arguments we
apply to this function with the type of the arguments from the type signature.

In this case, we try to apply two arguments to `.`:

1. `Structure :: String -> Structure`
2. `el "p" :: String -> String`

And luckily `.` expects two arguments with the types:

1. `b -> c`
2. `a -> b`

> Note: Applying a function with more arguments than it expects is a type error.

Since the `.` operator takes at least the number of arguments we supply, we continue
to the next phase of type-checking: matching the types of the inputs with the types
of the expected inputs (from the type signature of the operator).

When we match two types, we are checking for *equivalence* between them. There are a few
possible scenarios here:

1. When the two types are **concrete** (as opposed to type variables)
   and **simple**, like `Int` and `Bool`,
   we check if they are the same. If they are, they type check and we continue.
   If they aren't, they don't type check and we throw an error.
2. When the two types we match are more **complex** (for example both are functions),
   we try to match their inputs and outputs (in case of functions). If the inputs and outputs
   match, then the two types match.
3. There is a special case when one of the types is a **type variable** -
   in this case we treat the matching process like an equation and we write it down somewhere.
   The next time we see this type variable, we *replace it with its match in the equation*.
   Think about this like *assigning* a type *variable* with a *value*.

In our case, we want to match (or check the equivalence of) these types:

1. `String -> Structure` with `b -> c`
2. `String -> String` with `a -> b`

Let's do this one by one, starting with (1) - matching `String -> Structure` and `b -> c`:

1. Because the two types are complex, we check that they are both functions, and match their
   inputs and outputs: `String` with `b`, and `Structure` with `c`.
2. Because `b` is a *type variable*, we mark down somewhere that `b` should
   be equivalent to `String`.
   We write `b ~ String` (we use `~` to denote equivalence).
3. We match `Structure` and `c`, same as before, we write down that `c ~ Structure`.

No problem so far, let's try matching `String -> String` with `a -> b`:

1. The two types are complex, we see that both are functions so we match
   their inputs and outputs.
2. Matching `String` with `a` - we write down that `a ~ String`.
3. Matching `String` with `b` - we remember that we have already written
   about `b` - looking back we see that we already noted that `b ~ String`.
   We need to replace `b` with the type that we wrote down before and
   check it against this type, so we match `String` with `String` 
   which, fortunately, type-check because they are the same.

So far so good. We've type-checked the expression and discovered the following equivalences 
about the type variables in it:

1. `a ~ String`
2. `b ~ String`
3. `c ~ Structure`

Now, when asking what is the type of the expression:

```hs
p_ = Structure . el "p"
```

We say that it is the type of `.` after *replacing* the type variables using the equations we found
and *removing* the inputs we applied to it, so we started with:

```hs
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

Then we replaced the type variables:

```hs
(.) :: (String -> Structure) -> (String -> String) -> (String -> Structure)
```

And removed the two arguments when we applied the function:

```hs
Structure . el "p" :: String -> Structure
```

And we got the type of the expression!

Fortunately, Haskell is able to do this process for us. But when Haskell complains
that our types fail to type-check and we don't understand exactly why, going through this process
can help us understand where the types do not match, and then we can figure out how to solve it.


> **Note**: If we use a *parametrically polymorphic* function more than once,
> or use different functions that have similar type variable names,
> the type variables don't have to match in all instances simply because they share a name.
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
> The first `id` takes a `Char` as argument, and its `a` is equivalent to `Char`.
> The second `id` takes an `Int` as argument, and its *distinct* `a` is equivalent to `Int`.
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
> Will not type check. Try it!

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

## Converting back `Html` to `String`

After constructing a valid `Html` value, we want to be able to
print it to the output so we can display it in our browser.
For that, we need a function that takes an `Html` and converts it to a `String`, which we can then pass to `putStrLn`.

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

## `type`

Let's look at one more way to give new names to types.

A `type` definition looks really similar to a `newtype` definition - the only
difference is that we reference the type name directly without a constructor:

```
type <type-name> = <existing-type>
```

For example in our case we can write:

```hs
type Title = String
```

`type`, in contrast with `newtype`, is just a type name alias.
When we declare `Title` as a *type alias* of `String`,
we mean that `Title` and `String` are interchangeable,
and we can use one or the other whenever we want:

```hs
"hello" :: Title

"hello" :: String
```

Both are valid in this case.

We can sometimes use `type`s to give a bit more clarity to our code,
but they are much less useful than `newtype`s which allow us to
*distinguish* two types with the same type representation.

## The rest of the owl

---

Try changing the code we wrote in previous chapters to use the new types we created.

> **Tips**
>
> We can combine `makeHtml` and `html_`, and remove `body_` `head_` and `title_`
> by calling `el` directly in `html_`, which can now have the type
> `Title -> Structure -> Html`.
> This will make our HTML EDSL less flexible but more compact.
>
> Alternatively, we could create `newtype`s for `HtmlHead` and `HtmlBody` and
> pass those to `html_`, and we might do that at later chapters, but I've chosen
> to keep the API a bit simple for now, we can always refactor later!

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
      (h1_ "Heading")
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

## Are we safe yet?

We have made some progress - now we can't write `"Hello"`
where we'd expect either a paragraph or a heading, but we can still
write `Structure "hello"` and get something that isn't a
paragraph or a heading. So while we made it harder for the user
to make mistakes by accident, we haven't really been able to **enforce
the invariants** we wanted to enforce in our library.

Next we'll see how we can make expressions such as `Structure "hello"` illegal
as well using *modules* and *smart constructors*.
