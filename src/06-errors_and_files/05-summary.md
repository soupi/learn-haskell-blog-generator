# Summary

This was quite a section. Let's recount the things we've learned.

We discussed several ways to handle errors in Haskell:

1. Encoding errors as a data type and using the `Either` type to encode "a value or an error".
   Useful approach for uneffectful code
2. Using `ExceptT` when we want to combine the approach in (1) on top on an existing
   type with monadic capabilities
3. Using exceptions for IO code

We've also learned a few new abstractions and techniques:

1. The `Traversable` type class, for data structures that can be traversed from left to right
   such as linked lists, binary trees and `Map`s.
   Pretty useful when combined with another applicative functor type like `Either` or `IO`
2. The `Monad` type class extends the `Applicative` type class with the `join :: m (m a) -> m a`
   function. We learned that `Either` implements this type class interface and so does `IO`
3. The `MonadTrans` type class for *monad transformers* for types that take other monads as inputs
   and provide a monadic interface (`>>=`, do notation, etc.) while combining both their capabilities.
   We saw how to stack an `Either`-like monad transformer, `ExceptT`, on top of `IO`

We are almost done. Only a couple more things left to do with this project. Let's go!

> You can view the git commit of
> [the changes we've made](https://github.com/soupi/learn-haskell-blog-generator/commit/a08d148d981fa00cb7025f1b651d7b75084dd1ae)
> and the [code up until now](https://github.com/soupi/learn-haskell-blog-generator/tree/a08d148d981fa00cb7025f1b651d7b75084dd1ae).
