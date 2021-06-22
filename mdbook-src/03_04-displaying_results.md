# Displaying the parsing results (typeclasses)

We want to be able to print a textual representation of values
of our `Document` type. There are a few ways to do that:

1. Write our own function of type `Document -> String` which we could then print, or
2. Have Haskell write one for us

Haskell provides us with mechanism that can automatically generates the implementation of a
*typeclass* function called `show`, that will convert our type to `String`.

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
  = Header Int String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving Show
```

Now we can use the function `show :: Show a => a -> String` for any
type that implements String. For example, with print:

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

Typeclasses often come with "rules" or "laws" that instances should satisfy,
the purpose of these laws is to provide *predictable behaviour* across
instances, so that when we run into a new instance we can be confident
that it will behave in a certain expected way.

As an example, let's look at the `Semigroup` typeclass:

```hs
class Semigroup a where
  (<>) :: a -> a -> a
```

This typeclass provides a common interface for types with an operation `<>`
that can combine two values into one in some way.

This typeclass also mentions that this `<>` operation should be associative,
meaing that these two sides should do the same thing:

```hs
x <> (y <> z) = (x <> y) <> z
```

An example of a lawful instance of `Semigroup` is lists with the append operation (`++`):

```hs
instance Semigroup [a] where
  (<>) = (++)
```

Unfortunately the Haskell type system cannot "prove" that instances
satisfy these laws, but as a community we often shun unlawful instances.

We have [previously](02_04-safer_construction.html#appending-htmlstructure)
created a function that looks like this for our `Html` EDSL!
We can add a `Semigroup` instance for our `HtmlStructure` data type
and have a nicer to use API!

Please do this and remove the `append_` function from the API.
