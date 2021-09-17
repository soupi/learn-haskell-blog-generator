# Displaying the parsing results (type classes)

We want to be able to print a textual representation of values
of our `Document` type. There are a few ways to do that:

1. Write our own function of type `Document -> String` which we could then print, or
2. Have Haskell write one for us

Haskell provides us with mechanism that can automatically generates the implementation of a
*type class* function called `show`, that will convert our type to `String`.

The type of the function `show` looks like this:

```hs
show :: Show a => a -> String
```

This is something new we haven't seen before. Between `::` and `=>`
you see what is called a __type class constraint__ on the type `a`. What
we say in this signature, is that the function `show` can work on any
type that is a member of the type class `Show`.

Type classes is a feature in Haskell that allows us to declare common
interface for different types. In our case, Haskell's standard library
defines the type class `Show` in the following (this is a simplified
version but good enough for our purposes):

```hs
class Show a where
  show :: a -> String
```

This means that we have now declared a common interface for Haskell
types that want it. Any time that wants to implement it needs to
define an *instance* for the type class. For example:

```hs
instance Show Bool where
  show x =
    case x of
      True -> "True"
      False -> "False"
```

If all the types we use while defining our data type already implement
this, we can automatically derive it by adding `deriving Show` at the
end of the data definition.

```hs
data Structure
  = Header Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving Show
```

Now we can use the function `show :: Show a => a -> String` for any
type that implements an instance of the `Show` type class. For example, with print:

```hs
print :: Show a => a -> IO ()
print = putStrLn . show
```

We can first convert our type to `String` and then write it to the
standard output.

And because lists also implement `Show` for any element type that has
a `Show` instance, we can now print `Document`s, because they are just
aliases for `[Structure]`. Try it!

There are many type classes Haskellers use everyday. A couple more are
`Eq` for equality and `Ord` for ordering.

Type classes often come with "rules" or "laws" that instances should satisfy,
the purpose of these laws is to provide *predictable behaviour* across
instances, so that when we run into a new instance we can be confident
that it will behave in a certain expected way.

As an example, let's look at the `Semigroup` type class:

```hs
class Semigroup a where
  (<>) :: a -> a -> a
```

This type class provides a common interface for types with an operation `<>`
that can combine two values into one in some way.

This type class also mentions that this `<>` operation should be associative,
meaning that these two sides should evaluate to the same result:

```
x <> (y <> z) = (x <> y) <> z
```

An example of a lawful instance of `Semigroup` is lists with the append operation (`++`):

```hs
instance Semigroup [a] where
  (<>) = (++)
```

Unfortunately the Haskell type system cannot "prove" that instances
satisfy these laws, but as a community we often shun unlawful instances.

Many data types (together with their respective operations) can
form a `Semigroup` (or any other type class), and instances
don't even have to look similar or have a common analogy/metaphor.

**Type classes are often just _interfaces_ with _laws_** (or expected behaviour if you will).
Approaching them with this mindset can be very liberating!

To put it in a differently, **type classes can be used to create abstractions** -
interfaces with laws/expected behaviour where we don't actually care about the
concrete details of the underlying type, just that it *implements a certain
API and behaves as expected*.

We have [previously](02_04-safer_construction.html#appending-htmlstructure)
created a function that looks like this for our `Html` EDSL!
We can add a `Semigroup` instance for our `Structure` data type
and have a nicer to use API!

Exercise: Please do this and remove the `append_` function from the API.

<details>
  <summary>Solution</summary>

Replace this:

```hs
append_ :: Structure -> Structure -> Structure
append_ c1 c2 =
  Structure (getStructureString c1 <> getStructureString c2)
```

With this:

```hs
instance Semigroup Structure where
  (<>) c1 c2 =
    Structure (getStructureString c1 <> getStructureString c2)
```

And remove the export of `append_` in `Html.hs`. You won't need to further export anything
as type class instances are exported automatically.

You will also need replace the usage of `append_` with `<>` in `hello.hs`.

</details>
