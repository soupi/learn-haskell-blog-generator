# Frequently asked questions

> Got a question? You can ask in the [book's issue tracker](https://github.com/soupi/learn-haskell-blog-generator/issues)!

## General questions

### Why should I learn Haskell

I've written a couple of articles on the topic:

- [Consider Haskell](https://gilmi.me/blog/post/2020/04/28/consider-haskell) (Alternative title, 'What can I do with Haskell?')
- [7 things I learned from Haskell](https://gilmi.me/blog/post/2022/12/13/learned-from-haskell)

### How to install editor tools

As far as I know, the most recommended setup today for Haskell development is using
VSCode or [VSCodium](https://vscodium.com/) together with the
marketplace [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell).

The Haskell extension uses [haskell-language-server](https://github.com/haskell/haskell-language-server)
which can be installed via [GHCup](https://www.haskell.org/ghcup/) or even via the Haskell extension itself.

If you already have a preferred editor,
[see if HLS supports it](https://haskell-language-server.readthedocs.io/en/latest/configuration.html#configuring-your-editor),
or alternatively use [GHCid](https://github.com/ndmitchell/ghcid#readme)
which provides rapid feedback independently from an editor.

### How to learn new things

The Haskell community keeps marching forward, developing new libraries, tools and techniques
as well as creating new material for older concepts.
The [Haskell planetarium](https://haskell.pl-a.net) aggregates feeds from several communities into
one page, as well as a [Haskell Weekly newsletter](https://haskellweekly.news/).
You might also find the quite a bit of Haskell presence on
[Twitter](https://twitter.com/search?q=%23Haskell&src=typeahead_click)!

## Debugging

### How to debug Haskell code

Most imperative languages provide a step debugger. While the
[GHCi debugger](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#the-ghci-debugger),
exists it is not particularly easy to use, especially because of Haskell's lazy evaluation where things
might not evaluated at the order we might intuitively expect. Because of that,
Haskellers tend to use
[trace debugging](https://hackage.haskell.org/package/base-4.16.4.0/docs/Debug-Trace.html#g:1) and
equational reasoning. With trace debugging, we try to *verify our assumptions* about the code -
we use the various `trace` functions as a "hack" to print variables, functions inputs, functions output
or even just say "got here", from anywhere at the code.

After finding something that does not match our assumptions, such as unexpected input or output
of a function, we try to think what piece of code could be responsible for the discrepancy, or even use
trace debugging again to pinpoint the exact location, and try to use "equational reasoning" to
evaluate the offending code that betrayed our expectations. If it's easy to do, we try running
the function in `ghci` with different inputs to check our assumptions as well.

Because Haskell focuses on immutability, composibility and using types to eliminate many
classes of possible errors, "local reasoning" becomes possible, and trace debugging
becomes a viable strategy for debugging Haskell programs.

### How to understand type errors

GHC type errors are often not the most friendly errors messages, but they mean well! They are just
trying to help us find inconsistencies in our code - often with regards to type usage, they help us
avoid making errors.

When you run into error messages, start by reading the messages themselves carefully
until you get used to them, and then the offending code hinted by the error message.
As you gain experience, it is likely that the most important part of an error will be the location
of the offending code, and by reading the code we can find the error without the actual error message.

Adding type signatures and annotations to test your understanding of the types also helps greatly.
We can even ask GHC for the expected type in a certain place by using
[typed holes](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/typed_holes.html).

### My program is slow. Why?

There could be various reasons. From inefficient algorithms or
[unsuited data structures](https://github.com/soupi/haskell-study-plan#data-structures) for the task
in terms of time complexity of the common operations, to less efficient memory representations
(this is another reminder to use `Text` over `String` in most cases),
and laziness issues (again, the evaluation strategy!).

The [performance section](https://github.com/soupi/haskell-study-plan#performance) in my Haskell
study plan links to various resources on Haskell evaluation, profiling and case studies.

## Design

### How to structure programs

Start with the imperative shell functional core approach, define EDSLs with the combinator
pattern for logic if needed, use capabilities such as `State` locally if needed,
maybe add an environment configuration with `ReaderT`, see how it goes.

If that approach fails you, look at why it fails and examine other solutions according to your needs.

### How to model data

Modeling data using ADTs are usually the way to go. Often programmers coming from object oriented
background tend to look at type classes as a way to define methods similar to inheritance,
but this often isn't the right approach and ADTs with different constructors for different alternatives
go a long way. Remember that even OOP people often preach for composition over inheritance.

Use functions to define behavior on data rather than trying to couple the two together.
