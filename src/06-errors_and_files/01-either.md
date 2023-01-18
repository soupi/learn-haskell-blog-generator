# Handling errors with Either

There are quite a few ways to indicate and handle errors in Haskell.
We are going to look at one solution: using the type
[Either](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Either.html).
Either is defined like this:

```hs
data Either a b
  = Left a
  | Right b
```

Simply put, a value of type `Either a b` can contain either a value of type `a`,
or a value of type `b`.
We can tell them apart from the constructor used.

```hs
Left True :: Either Bool b
Right 'a' :: Either a Char
```

With this type, we can use the
`Left` constructor to indicate failure with some error value attached,
and the `Right` constructor with one type to represent success with the
expected result.

Since `Either` is polymorphic, we can use any two types to represent
failure and success. It is often useful to describe the failure modes
using an ADT.

For example, let's say that we want to parse a `Char` as a decimal digit
to an `Int`. This operation could fail if the Character is not a digit.
We can represent this error as a data type:

```hs
data ParseDigitError
  = NotADigit Char
  deriving Show
```

And our parsing function can have the type:

```hs
parseDigit :: Char -> Either ParseDigitError Int
```

Now when we implement our parsing function we can return `Left` on an error
describing the problem, and `Right` with the parsed value on successful parsing:


```hs
parseDigit :: Char -> Either ParseDigitError Int
parseDigit c =
  case c of
    '0' -> Right 0
    '1' -> Right 1
    '2' -> Right 2
    '3' -> Right 3
    '4' -> Right 4
    '5' -> Right 5
    '6' -> Right 6
    '7' -> Right 7
    '8' -> Right 8
    '9' -> Right 9
    _ -> Left (NotADigit c)
```

`Either a` is also an instance of `Functor` and `Applicative`,
so we have some combinators to work with if we want to combine these
kind of computations.

For example, if we had three characters and we wanted to try and parse
each of them and then find the maximum between them, we could use the
applicative interface:

```hs
max3chars :: Char -> Char -> Char -> Either ParseDigitError Int
max3chars x y z =
  (\a b c -> max a (max b c))
    <$> parseDigit x
    <*> parseDigit y
    <*> parseDigit z
```


The `Functor` and `Applicative` interfaces of `Either a` allow us to
apply functions to the payload values and **delay** the error handling to a
later phase. Semantically, the first Either in order that returns a `Left`
will be the return value. We can see how this works in the implementation
of the applicative instance:

```hs
instance Applicative (Either e) where
    pure          = Right
    Left  e <*> _ = Left e
    Right f <*> r = fmap f r
```

At some point, someone will actually want to **inspect** the result
and see if we get an error (with the `Left` constructor) or the expected value
(with the `Right` constructor) and they can do that by pattern matching the result.

## Applicative + Traversable

The `Applicative` interface of `Either` is very powerful, and can be combined
with another abstraction called
[`Traversable`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Traversable.html#g:1) -
for data structures that can be traversed from left to right, like a linked list or a binary tree.
With these, we can combine an unspecified amount of values such as `Either ParseDigitError Int`,
as long as they are all in a data structure that implements `Traversable`.

Let's see an example:

```hs
ghci> :t "1234567"
"1234567" :: String
-- remember, a String is an alias for a list of Char
ghci> :info String
type String :: *
type String = [Char]
      -- Defined in ‘GHC.Base’

ghci> :t map parseDigit "1234567"
map parseDigit mystring :: [Either ParseDigitError Int]
ghci> map parseDigit "1234567"
[Right 1,Right 2,Right 3,Right 4,Right 5,Right 6,Right 7]

ghci> :t sequenceA
sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
-- Substitute `t` with `[]`, and `f` with `Either Error` for a specialized version

ghci> sequenceA (map parseDigit mystring)
Right [1,2,3,4,5,6,7]

ghci> map parseDigit "1a2"
[Right 1,Left (NotADigit 'a'),Right 2]
ghci> sequenceA (map parseDigit "1a2")
Left (NotADigit 'a')
```

The pattern of doing `map` and then `sequenceA` is another function called `traverse`:

```hs
ghci> :t traverse
traverse
  :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
ghci> traverse parseDigit "1234567"
Right [1,2,3,4,5,6,7]
ghci> traverse parseDigit "1a2"
Left (NotADigit 'a')
```

We can use `traverse` on any two types where one implements the `Applicative`
interface, like `Either a` or `IO`, and the other implements the `Traversable` interface,
like `[]` (linked lists) and
[`Map k`](https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map-Strict.html#t:Map)
(also known as a dictionary in other languages - a mapping from keys to values).
For example using `IO` and `Map`. Note that we can construct a `Map` data structure
from a list of tuples using the
[`fromList`](https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map-Strict.html#v:fromList)
function - the first value in the tuple is the key, and the second is the type.

```hs
ghci> import qualified Data.Map as M -- from the containers package

ghci> file1 = ("output/file1.html", "input/file1.txt")
ghci> file2 = ("output/file2.html", "input/file2.txt")
ghci> file3 = ("output/file3.html", "input/file3.txt")
ghci> files = M.fromList [file1, file2, file3]
ghci> :t files :: M.Map FilePath FilePath -- FilePath is an alias of String
files :: M.Map FilePath FilePath :: M.Map FilePath FilePath

ghci> readFiles = traverse readFile
ghci> :t readFiles
readFiles :: Traversable t => t FilePath -> IO (t String)

ghci> readFiles files
fromList [("output/file1.html","I'm the content of file1.txt\n"),("output/file2.html","I'm the content of file2.txt\n"),("output/file3.html","I'm the content of file3.txt\n")]
ghci> :t readFiles files
readFiles files :: IO (Map String String)
```

Above, we created a function `readFiles` that will take a mapping from *output file path*
to *input file path* and returns an IO operation that when run will read the input files
and replace their contents right there in the map! Surely this will be useful later.

## Multiple errors

Note, since `Either` has the kind `* -> * -> *` (it takes two type
parameters) `Either` cannot be an instance of `Functor` or `Applicative`:
instances of these type classes must have the
kind `* -> *`.
Remember that when we look at a type class function signature like:

```hs
fmap :: Functor f => (a -> b) -> f a -> f b
```

And we want to implement it for a specific type (in place of the `f`),
we need to be able to *substitute* the `f` with the target type. If we'd try
to do it with `Either` we would get:

```hs
fmap :: (a -> b) -> Either a -> Either b
```

And neither `Either a` or `Either b` are *saturated*, so this won't type check.
For the same reason if we'll try to substitute `f` with, say, `Int`, we'll get:

```hs
fmap :: (a -> b) -> Int a -> Int b
```

Which also doesn't make sense.

While we can't use `Either`, we can use `Either e`, which has the kind
`* -> *`. Now let's try substituting `f` with `Either e` in this signature:

```hs
liftA2 :: Applicative => (a -> b -> c) -> f a -> f b -> f c
```

And we'll get:

```hs
liftA2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
```

What this teaches us is that we can only use the applicative interface to
combine two *`Either`s with the same type for the `Left` constructor*.

So what can we do if we have two functions that can return different errors?
There are a few approaches, the most prominent ones are:

1. Make them return the same error type. Write an ADT that holds all possible
   error descriptions. This can work in some cases but isn't always ideal.
   For example a user calling `parseDigit` shouldn't be forced to
   handle a possible case that the input might be an empty string
2. Use a specialized error type for each type, and when they are composed together,
   map the error type of each function to a more general error type. This can
   be done with the function
   [`first`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-Bifunctor.html#v:first)
   from the `Bifunctor` type class

## Monadic interface

The applicative interface allows us to lift a function to work on multiple
`Either` values (or other applicative functor instances such as `IO` and `Parser`).
But more often than not, we'd like to use a value from one computation
that might return an error in another computation that might return an error.

For example, a compiler such has GHC operates in stages, such as lexical analysis,
parsing, type-checking, and so on. Each stage depends on the output of the stage
before it, and each stage might fail. We can write the types for these functions:

```hs
tokenize :: String -> Either Error [Token]

parse :: [Token] -> Either Error AST

typecheck :: AST -> Either Error TypedAST
```

We want to compose these functions so that they work in a chain. The output of `tokenize`
goes to `parse`, and the output of `parse` goes to `typecheck`.

We know that we can lift a function over an `Either` (and other functors),
we can also lift a function that returns an `Either`:

```hs
-- reminder the type of fmap
fmap :: Functor f => (a -> b) -> f a -> f b
-- specialized for `Either Error`
fmap :: (a -> b) -> Either Error a -> Either Error b

-- here, `a` is [Token] and `b` is `Either Error AST`:

> fmap parse (tokenize string) :: Either Error (Either Error AST)
```

While this code compiles, it isn't great, because we are building
layers of `Either Error` and we can't use this trick again with
`typecheck`! `typecheck` expects an `AST`, but if we try to fmap it
on `fmap parse (tokenize string)`, the `a` will be `Either Error AST`
instead.

What we would really like is to flatten this structure instead of nesting it.
If we look at the kind of values `Either Error (Either Error AST)` could have,
it looks something like this:

- `Left <error>`
- `Right (Left error)`
- `Right (Right <ast>)`

---

**Exercise**: What if we just used pattern matching for this instead? How would this look like?

<details><summary>Solution</summary>

```hs
case tokenize string of
  Left err ->
    Left err
  Right tokens ->
    case parse tokens of
      Left err ->
        Left err
      Right ast ->
        typecheck ast
```

If we run into an error in a stage, we return that error and stop. If we succeed, we
use the value on the next stage.

</details>

---

Flattening this structure for `Either` is very similar to that last part - the body
of the `Right tokens` case:

```hs
flatten :: Either e (Either e a) -> Either e a
flatten e =
  case e of
    Left l -> Left l
    Right x -> x
```

Because we have this function, we can now use it on the output of
`fmap parse (tokenize string) :: Either Error (Either Error AST)`
from before:

```
> flatten (fmap parse (tokenize string)) :: Either Error AST
```

And now we can use this function again to compose with `typecheck`:

```hs
> flatten (fmap typecheck (flatten (fmap parse (tokenize string)))) :: Either Error TypedAST
```

This `flatten` + `fmap` combination looks like a recurring pattern which
we can combine into a function:

```hs
flatMap :: (a -> Either e b) -> Either a -> Either b
flatMap func val = flatten (fmap func val)
```

And now we can write the code this way:

```hs
> flatMap typecheck (flatMap parse (tokenize string)) :: Either Error TypedAST

-- Or using backticks syntax to convert the function to infix form:
> typecheck `flatMap` parse `flatMap` tokenize string

-- Or create a custom infix operator: (=<<) = flatMap
> typeCheck =<< parse =<< tokenize string
```


This function, `flatten` (and `flatMap` as well), have different names in Haskell.
They are called
[`join`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Monad.html#v:join)
and [`=<<`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Monad.html#v:-61--60--60-)
(pronounced "reverse bind"),
and they are the essence of another incredibly useful abstraction in Haskell.

If we have a type that can implement:

1. The `Functor` interface, specifically the `fmap` function
2. The `Applicative` interface, most importantly the `pure` function
3. This `join` function

They can implement an instance of the `Monad` type class.

With functors, we were able to "lift" a function to work over the type implementing the functor type class:

```hs
fmap :: (a -> b) -> f a -> f b
```

With applicative functors we were able to "lift" a function of multiple arguments
over multiple values of a type implementing the applicative functor type class,
and also lift a value into that type:

```hs
pure :: a -> f a

liftA2 :: (a -> b -> c) -> f a -> f b -> f c
```

With monads we can now flatten (or, "join" in Haskell terminology) types that implement
the `Monad` interface:

```hs
join :: m (m a) -> m a

-- this is =<< with the arguments reversed, pronounced "bind"
(>>=) :: m a -> (a -> m b) -> m b
```

With `>>=` we can write our compilation pipeline from before in a left-to-right
manner, which seems to be more popular for monads:

```hs
> tokenize string >>= parse >>= typecheck
```

We have already met this function before when we talked about `IO`. Yes,
`IO` also implements the `Monad` interface. The monadic interface for `IO`
helped us with creating a proper ordering of effects.

The essence of the `Monad` interface is the `join`/`>>=` functions, and as we've seen
we can implement `>>=` in terms of `join`, we can also implement `join` in terms
of `>>=` (try it!).

The monadic interface can mean very different things for different types. For `IO` this
is ordering of effects, for `Either` it is early cutoff,
for [`Logic`](https://hackage.haskell.org/package/logict-0.7.1.0) this means backtracking computation, etc.

Again, don't worry about analogies and metaphors, focus on the API and the
[laws](https://wiki.haskell.org/Monad_laws).

> Hey, did you check the monad laws? left identity, right identity and associativity? We've already
> discussed a type class with exactly these laws - the `Monoid` type class. Maybe this is related
> to the famous quote about monads being just monoids in something something...

### Do notation?

Remember the [do notation](../05-glue/02-io.html#do-notation)? Turns out it works for any type that is
an instance of `Monad`. How cool is that? Instead of writing:

```hs
pipeline :: String -> Either Error TypedAST
pipeline string =
  tokenize string >>= \tokens ->
    parse tokens >>= \ast ->
      typecheck ast
```

We can write:

```hs
pipeline :: String -> Either Error TypedAST
pipeline string = do
  tokens <- tokenize string
  ast <- parse tokens
  typecheck ast
```

And it will work! Still, in this particular case `tokenize string >>= parse >>= typecheck`
is so concise it can only be beaten by using
[>=>](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Monad.html#v:-62--61--62-)
or
[<=<](https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Monad.html#v:-60--61--60-):

```hs
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c

-- compare with function composition:
(.) ::              (b ->   c) -> (a ->   b) -> a ->   c
```

```hs
pipeline  = tokenize >=> parse >=> typecheck
```

or

```hs
pipeline = typecheck <=< parse <=< tokenize
```

Haskell's ability to create very concise code using abstractions is
great once one is familiar with the abstractions. Knowing the monad abstraction,
we are now already familiar with the core composition API of many libraries - for example:

- [Concurrent](https://hackage.haskell.org/package/stm)
  and [asynchronous programming](https://hackage.haskell.org/package/async)
- [Web programming](https://gilmi.me/blog/post/2020/12/05/scotty-bulletin-board)
- [Testing](http://hspec.github.io/)
- [Emulating stateful computation](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Lazy.html#g:2)
- [sharing environment between computations](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Reader.html#g:2)
- and many more.

## Summary

Using `Either` for error handling is useful for two reasons:

1. We encode possible errors using types, and we **force users to acknowledge and handle** them, thus
   making our code more resilient to crashes and bad behaviours
2. The `Functor`, `Applicative`, and `Monad` interfaces provide us with mechanisms for
   **composing** functions that might fail (almost) effortlessly - reducing boilerplate while
   maintaining strong guarantees about our code, and delaying the need to handle errors until
   it is appropriate
