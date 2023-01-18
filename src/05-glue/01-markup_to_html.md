# Converting Markup to HTML

One key part is missing before we can glue everything together, and that is
to convert our `Markup` data types to `Html`.

We'll start by creating a new module and import both the `Markup` and the `Html` modules.

```hs
module Convert where

import qualified Markup
import qualified Html
```

## Qualified Imports

This time, we've imported the modules qualified. Qualified imports means that
instead of exposing the names that we've defined in the imported module to
the general module name space, they now have to be prefixed with the module name.

For example, `parse` becomes `Markup.parse`.
If we would've imported `Html.Internal` qualified, we'd have to write
`Html.Internal.el` which is a bit long.

We can also give the module a new name with the `as` keyword:

```hs
import qualified Html.Internal as HI
```

And write `HI.el` instead.

I like using qualified imports because readers do not have to guess where a
name comes from. Some modules are even designed to be imported qualified.
For example, the APIs of many container types such as maps, sets, and vectors, are very similar.
If we want to use multiple containers in a single module we pretty much have
to use qualified imports so that when we write a function such as `singleton`,
which creates a container with a single value, GHC will know which `singleton`
function we are referring to.

Some people prefer to use import lists instead of qualified imports,
because qualified names can be a bit verbose and noisy.
I will often prefer qualified imports to import lists, but feel free to
try both solutions and see which fits you better.
For more information about imports,
see this [wiki article](https://wiki.haskell.org/Import).

## Converting `Markup.Structure` to `Html.Structure`

Converting a markup structure to an HTML structure is mostly straightforward
at this point, we need to pattern match on the markup structure and use
the relevant HTML API.

```hs
convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading 1 txt ->
      Html.h1_ txt

    Markup.Paragraph p ->
      Html.p_ p

    Markup.UnorderedList list ->
      Html.ul_ $ map Html.p_ list

    Markup.OrderedList list ->
      Html.ol_ $ map Html.p_ list

    Markup.CodeBlock list ->
      Html.code_ (unlines list)
```

Notice that running this code with `-Wall` will reveal that the pattern matching
is *non-exhaustive*. This is because we don't currently have a way to build
headings that are not `h1`. There are a few ways to handle this:

- Ignore the warning - this will likely fail at runtime one day and the user will be sad
- Pattern match other cases and add a nice error with the `error` function - it has
  the same disadvantage above, but will also no longer notify of the unhandled
  cases at compile time
- Pattern match and do the wrong thing - user is still sad
- Encode errors in the type system using `Either`, we'll see how to do this in later
  chapters
- Restrict the input - change `Markup.Heading` to not include a number but rather
  specific supported headings. This is a reasonable approach
- Implement an HTML function supporting arbitrary headings. Should be straightforward
  to do

---

Exercise: Implement `h_ :: Natural -> String -> Structure`
which we'll use to define arbitrary headings (such as `<h1>`, `<h2>`, and so on).

<details><summary>Solution</summary>

```hs
import Numeric.Natural

h_ :: Natural -> String -> Structure
h_ n = Structure . el ("h" <> show n) . escape
```

Don't forget to export it from `Html.hs`!


</details>


Exercise: Fix `convertStructure` using `h_`.


<details><summary>Solution</summary>

```hs
convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt ->
      Html.h_ n txt

    Markup.Paragraph p ->
      Html.p_ p

    Markup.UnorderedList list ->
      Html.ul_ $ map Html.p_ list

    Markup.OrderedList list ->
      Html.ol_ $ map Html.p_ list

    Markup.CodeBlock list ->
      Html.code_ (unlines list)
```

</details>

---

## Document -> Html

In order to create an `Html` document, we need to use the `html_` function.
This function expects two things: a `Title`, and a `Structure`.

For a title we could just supply it from outside using the file name.

In order to convert our markup `Document` (which is a list of markup `Structure`)
to an HTML `Structure`, we need to convert each markup `Structure` and then
concatenate them together.

We already know how to convert each markup `Structure`, we can use the
`convertStructure` function we wrote and `map`. This will provide
us with the following function:

```
map convertStructure :: Markup.Document -> [Html.Structure]
```

To concatenate all of the `Html.Structure`, we could try to write a recursive
function. However we will quickly run into an issue
with the base case, what to do when the list is empty?

We could just provide dummy `Html.Structure` that represents an empty
HTML structure.

Let's add this to `Html.Internal`:

```hs
empty_ :: Structure
empty_ = Structure ""
```

---

Now we can write our recursive function. Try it!

<details><summary>Solution</summary>

```hs
concatStructure :: [Structure] -> Structure
concatStructure list =
  case list of
    [] -> empty_
    x : xs -> x <> concatStructure xs
```

</details>

---

Remember the `<>` function we implemented as an instance of the `Semigroup`
type class? We mentioned that `Semigroup` is an **abstraction** for things
that implements `(<>) :: a -> a -> a`, where  `<>` is associative
(`a <> (b <> c) = (a <> b) <> c`).

It turns out that having an instance of `Semigroup` and also having a value that represents
an "empty" value is a fairly common pattern. For example a string can be concatenated, 
and the empty string can serve as an "empty" value.
And this is actually a well known **abstraction** called **monoid**.

## Monoids

Actually, "empty" isn't a very good description of what we want,
and isn't very useful as an abstraction. Instead, we can describe it as
an "identity" element, which satisfies the following laws:

- `x <> <identity> = x`
- `<identity> <> x = x`

In other words, if we try to use this "empty" - this identity value,
as one argument to `<>`, we will always get the other argument back.

For `String`, the empty string, `""`, satisfies this:

```hs
"" <> "world" = "world"
"hello" <> "" = "hello"
```

This is of course true for any value we'd write and not just "world" and "hello".

Actually, if we move out of the Haskell world for a second, even integers
with `+` as the associative binary operations `+` (in place of `<>`)
and `0` in place of the identity member form a monoid:

```hs
17 + 0 = 17
0 + 99 = 99
```

So integers together with the `+` operation form a semigroup, and
together with `0` form a monoid.

We learn new things from this:

1. A monoid is a more specific abstraction over semigroup, it builds on it
   by adding a new condition (the existence of an identity member)
2. This abstraction can be useful! We can write a general `concatStructure`
   that could work for any monoid

And indeed, there exists a type class in `base` called `Monoid` which has
`Semigroup` as a **super class**.

```hs
class Semigroup a => Monoid a where
  mempty :: a
```

> Note: this is actually a simplified version. The
> [actual](https://hackage.haskell.org/package/base-4.16.4.0/docs/Prelude.html#t:Monoid)
> is a bit more complicated because of backwards compatibility and performance reasons.
> `Semigroup` was actually introduced in Haskell after `Monoid`!

We could add an instance of `Monoid` for our HTML `Structure` data type:


```hs
instance Monoid Structure where
  mempty = empty_
```

And now, instead of using our own `concatStructure`, we can use the library function:

```hs
mconcat :: Monoid a => [a] -> a
```

Which could theoretically be implemented as:

```hs
mconcat :: Monoid a => [a] -> a
mconcat list =
  case list of
    [] -> mempty
    x : xs -> x <> mconcat xs
```

Notice that because `Semigroup` is a *super class* of `Monoid`,
we can still use the `<>` function from the `Semigroup` class
without adding the `Semigroup a` constraint to the left side of `=>`.
By adding the `Monoid a` constraint we implicitly add a `Semigroup a`
constraint as well!

This `mconcat` function is very similar to the `concatStructure` function,
but this one works for any `Monoid`, including `Structure`!
Abstractions help us identify common patterns and **reuse** code!

> Side note: integers with `+` and `0` aren't actually an instance of `Monoid` in Haskell.
> This is because integers can also form a monoid with `*` and `1`! But **there can only
> be one instance per type**. Instead, two other `newtype`s exist that provide that
> functionality, [Sum](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Monoid.html#t:Sum)
> and [Product](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Monoid.html#t:Product).
> See how they can be used in `ghci`:
>
> ```hs
> ghci> import Data.Monoid
> ghci> Product 2 <> Product 3 -- note, Product is a data constructor
> Product {getProduct = 6}
> ghci> getProduct (Product 2 <> Product 3)
> 6
> ghci> getProduct $ mconcat $ map Product [1..5]
> 120
> ```

## Another abstraction?

We've used `map` and then `mconcat` twice now. Surely there has to be a function
that unifies this pattern. And indeed, it is called
[`foldMap`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Foldable.html#v:foldMap),
and it works not only for lists, but also for any data structure that can be "folded",
or "reduced", into a summary value. This abstraction and type class is called **Foldable**.

For a simpler understanding of `Foldable`, we can look at `fold`:

```hs
fold :: (Foldable t, Monoid m) => t m -> m

-- compare with
mconcat :: Monoid m            => [m] -> m
```

`mconcat` is just a specialized version of `fold` for lists.
And `fold` can be a used for any pair of a data structure that implements
`Foldable` and a payload type that implements `Monoid`. This
could be `[]` with `Structure`, or `Maybe` with `Product Int`, or
your new shiny binary tree with `String` as the payload type. But note that
the `Foldable` type must be of *kind* `* -> *`. So for example `Html`
cannot be a `Foldable`.

`foldMap` is a function that allows us to apply a function to the
payload type of the `Foldable` type right before combining them
with the `<>` function.

```hs
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

-- compare to a specialized version with:
-- - t ~ []
-- - m ~ Html.Structure
-- - a ~ Markup.Structure
foldMap
  :: (Markup.Structure -> Html.Structure)
  -> [Markup.Structure]
  -> Html.Structure
```

True to its name, it really "maps" before it "folds". You might pause here
and think "this 'map' we are talking about isn't specific for lists, maybe
that's another abstraction?", yes. It is actually a very important and
fundamental abstraction called `Functor`.
But I think we had enough abstractions for this chapter.
We'll cover it in a later chapter!

## Finishing our conversion module

Let's finish our code by writing `convert`:

```hs
convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure
```

Now we have a full implementation and are able to convert markup documents
to HTML:

```hs
-- Convert.hs
module Convert where

import qualified Markup
import qualified Html

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt ->
      Html.h_ n txt

    Markup.Paragraph p ->
      Html.p_ p

    Markup.UnorderedList list ->
      Html.ul_ $ map Html.p_ list

    Markup.OrderedList list ->
      Html.ol_ $ map Html.p_ list

    Markup.CodeBlock list ->
      Html.code_ (unlines list)
```

## Summary

We learned about:

- Qualified imports
- Ways to handle errors
- The `Monoid` type class and abstraction
- The `Foldable` type class and abstraction

Next, we are going to glue our functionality together and learn about
I/O in Haskell!

> You can view the git commit of
> [the changes we've made](https://github.com/soupi/learn-haskell-blog-generator/commit/ad34f2264e9114f2d7436ff472c78da47055fcfe)
> and the [code up until now](https://github.com/soupi/learn-haskell-blog-generator/tree/ad34f2264e9114f2d7436ff472c78da47055fcfe).
