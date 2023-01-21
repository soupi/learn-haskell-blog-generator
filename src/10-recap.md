# Recap

In this book we've implemented a very simple static blog generator while learning Haskell as we go.

- We've learned about basic Haskell building blocks, such as definitions, functions,
  types, modules, recursion, pattern matching, type classes, IO and exceptions.
- We've learned about [EDSLs](./03-html/03-edsls.html) and used the *combinator pattern* to implement
  a composable html generation library.
- We've learned how to leverage types, modules and smart constructors
  to [make invalid states unrepresentable](./03-html/04-safer_construction.html).
- We've learned how to represent complex data using [ADTs](./04-markup/01-data_type.html).
- We've learned how to use [pattern matching](./04-markup/04-parsing_02.html#pattern-matching) to transform ADTs,
  and how to use [recursion](./04-markup/02-parsing_01.html#recursion-and-accumulating-information) to solve problems.
- We've used the *functional core, imperative shell* approach to build a program that handles IO and applies
  our domain logic to user inputs.
- We've learned about abstractions such as [monoids](./05-glue/01-markup_to_html.html#monoids),
  [functors](./05-glue/04-optparse.html#functor) and [monads](./06-errors_and_files/01-either.html#monadic-interface),
  and how they can help us reuse code and convey information about shared interfaces.
- We've learned how to create fancy [command-line interfaces](./05-glue/04-optparse.html), [write tests](./08-testing.html),
  and [generate documentation](./09-documentation.html).

While Haskell is a very big and complex language, and there's always more to be learned,
I think we've reached an important milestone where
you can start building your own Haskell projects and be productive with Haskell!

This is a good time to celebrate and pat yourself on the back for getting this far! Great job, you!

If you'd like to learn even more about Haskell and continue your Haskell journey
beyond this book, check out the appendix sections [Where to go next](./11-next.md) and the [FAQ](./12-faq.md).

## Thank you!

Thank you for reading this book. I hope you enjoyed it and found Haskell interesting.

I would very much like to hear your feedback. If you'd like, you could leave your
feedback at this book's
[GitHub issue board](https://github.com/soupi/learn-haskell-blog-generator/issues),
or you could reach me directly on [mastodon](https://fosstodon.org/@suppi) or via email.
You can find my contact information [on my website](https://gilmi.me).

If you liked this book, do let me know - your kind words mean a lot.

> Finally, if you *really* liked this book and would like to support future passion projects
> like it, you can [support me directly via Ko-fi](https://ko-fi.com/gilmi).

Thank you and good luck with your next Haskell project!
