# Embedded Domain-Specific Languages

Right off the bat, we run into a common pattern in Haskell: creating
Embedded Domain-Specific Languages (EDSLs for short).

Domain-specific languages (DSLs) are specialized programming languages that are
tailored to specific domains, in contrast to general-purpose languages,
which try to work well in many domains.

A few examples of DSLs are:

- make - for defining build systems
- DOT - for defining graphs
- Sed - for defining text transformations
- CSS - for defining styling
- HTML - for defining web pages

An *embedded* domain-specific language is a little language that is
embedded inside another programming language, making a program written in
the EDSL a valid program in the language it was written in.

The little HTML library we've been writing can be considered an EDSL.
It is used specifically for building web pages (by returning HTML strings),
and is valid Haskell code!

In Haskell, we frequently create and use EDSLs to express domain-specific
logic. We have EDSLs for concurrency, command-line options parsing, JSON and HTML,
creating build systems, writing tests, and many more.

Specialized languages are useful because they can solve specific problems in
a concise (and often safe) way, and by embedding, we get to use the full power of
the host language for our domain logic, including syntax highlighting and
various tools available for the language.

The drawback of embedding domain-specific languages is that we have to adhere to
the rules of the programming language we embed in, such as syntactic and semantic rules.

Some languages alleviate this drawback by providing meta-programming capabilities
in the form of macros or other features to extend the language.
And while Haskell does provide such capabilities as well, it is also expressive and concise
enough that many EDSLs do not need them.

Instead, many Haskell EDSLs use a pattern called _the combinator pattern_:
They define *primitives* and *combinators* -
primitives are basic building blocks of the language,
and combinators are functions that combine primitives into more complex structures.

In our HTML EDSL, our primitives are functions such as `html_` and `title_`
that can be used to create a single HTML node, and we pass other
constructed nodes as input to these functions, and combine them into a more complex
structure with the append function `<>`.

There are still a few tricks we can use to make our HTML EDSL better:

1. We can use Haskell's type system to make sure we only construct *valid*
   HTML, so for example, we don't create a `<title>` node
   without a `<head>` node or have user content that 
   can include unescaped special characters,
   and throw a type error when the user tries to do something invalid.

2. Our HTML EDSL can move to its own module so it can be reused in multiple modules.

In the next few sections, we'll take a look at how to define our own types and
how to work with modules to make it harder to make errors, and a little bit
about linked lists in Haskell.
