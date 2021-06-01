# Richer HTML elements

Our HTML EDSL is nice, but it's missing some key features to be usable
for blogs - for example images and links are pretty important.
So we will make some changes to our HTML EDSL so we can include
text, links and images.

...

<!--

What we're going to do is, instead of passing `String` to `p_`, `h1_`
and other functions that represent HTML *structures*, we will
pass them a new type called `HtmlContent` which would represent
content rather than structure - things text, links and images.

We will also implement instances for a couple of type classes for this
new `HtmlContent` type. Namely `Semigroup` and `Monoid`.

These two type classes are named after their mathematical counter
part. Semigroup defines an operation that combines two values of a
type to a single value. This operation in Haskell is the function
`<>` (pronounced append) and we've already seen it in action for strings!
A Monoid is a Semigroup that also provides an "empty" value, `mempty`.

Often type classes will have laws attached to them, and any instance
should make sure these laws hold for it as well. These laws provide
predictable behaviour, and helps in reducing surprised and getting
intuition about how a type might behaves just by knowning that it has
an instance of some type class.

These kind of type classes and laws create proper *abstractions*.

For Semigroup for example, `<>` should be associative, meaning that
the placement of paranthesis will not change the result.
`(a <> b) <> c` should be exactly the same as `a <> (b <> c)`.

For example, Int could form a semigroup with the operator `+`, because
as we learned `+` combines two Int values to get an Int value, and
adding three integers can be done by first calculating the right it
and then the left, or the left one and then the right, and the result
will be the same.

Note that Int could also form a semigroup with the operator `*`, but
there can only be one instance per type and type class. So instead of
choosing, Haskell does not implement a semigroup for Int and instead
defines two `newtype`s in the standard library: `Sum` and `Product`,
which each has a type class instance of semigroup.

Monoid also has a law, `mempty` should be "neutral" with respect to
the Semigroup operation on that type (note that each type that wants
to implement a monoid instance should also implement
semigroup). Neutral means that doing `x <> mempty` or `mempty <> x`
will always result with `x`.

For example, for `Sum`, `mempty` is defined as 0, and for `Product` it
is defined as 1.

We will also defined another operator, `<+>`, that will make easy to
append two elements with a space between them. We will declare that
this new operator is an infix operator that is "right associative",
meaning Haskell should read `a <+> b <+> c` as `a <+> (b <+> c)`,
and it has precedence 5, which is higher than 4 but lower than
6. Presedence could be between 0 and 9 and defines which operation
should occur first. Function application for example has the highest
precedence over any operator, so for example `func x + func y` will
always mean `(func x) + (func y)`.

See this table to learn about the presedence of a few other operators:

https://www.haskell.org/onlinereport/decls.html#fixity

---

With that, instead of writing `p_ "hello"`, we now have to write
`p_ (txt_ "hello")`.

But: we can now also write:
```
p_ (txt_ "visit my" <+> link_ "https://gilmi.me" (txt_ "website"))
```

And get a nice mixture of text and hyper links, which we can use to
create an index page for all of our pages.

-->
