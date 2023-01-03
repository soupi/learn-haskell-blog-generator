# Testing

We want to add some tests to our blog generator. At the very least
a few regression tests to make sure that if we extend or change our markup parsing code,
HTML generation code, or translation from markup to HTML code, and make a mistake, we'll
have a safety net alerting us of issues.

We will use the [Hspec](https://hspec.github.io/) testing framework to write our tests.
There are other testing frameworks in Haskell, for example
[tasty](https://hackage.haskell.org/package/tasty), but I like Hspec's documentation,
so we'll use that.

## Initial setup

### Cabal file additions

We're going to define a new section in our `hs-blog-gen.cabal` file for our new test suite.
This section is called `test-suite` and it is fairly similar to the `library` and
`executable` sections.

The interfaces for how to define a test suite are described in the
[Cabal documentation](https://cabal.readthedocs.io/en/stable/cabal-package.html#test-suites).
We are going to use the `exitcode-stdio-1.0` interface. Let's go over the different settings
and options:

```cabal
test-suite hs-blog-gen-test
  import: common-settings
  type: exitcode-stdio-1.0
  hs-source-dirs: test
  main-is: Spec.hs

  -- other-modules:
  build-depends:
      base
    , hspec
    , hspec-discover
    , raw-strings-qq
    , hs-blog
  ghc-options:
    -O -threaded -rtsopts -with-rtsopts=-N
  build-tool-depends:
    hspec-discover:hspec-discover
```

- `hs-source-dirs: test` - The directory of the source files for the test suite
- `main-is: Spec.hs` - The entry point to the test suite
- `other-modules` - The modules in our test suite.
  Currently commented out because we haven't added any yet
- `build-depends` - The packages we are going to use:
  - [`base`](https://hackage.haskell.org/package/base) -
    The standard library for Haskell, as we've used before
  - [`hspec`](https://hackage.haskell.org/package/hspec) -
    The test framework we are going to use
  - [`hspec-discover`](https://hackage.haskell.org/package/hspec-discover) -
    Automatic discovery of Hspec tests
  - [`raw-strings-qq`](https://hackage.haskell.org/package/raw-strings-qq) -
    Additional syntax for writing raw string literals
  - `hs-blog` - Our library
- [`ghc-options`](https://cabal.readthedocs.io/en/stable/cabal-package.html#pkg-field-ghc-options) -
    Extra options and flags for GHC:
  - [`-O`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/using-optimisation.html#options-optimise) -
    Compile with optimizations
  - [`-threaded`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/phases.html#ghc-flag--threaded) -
    Use the multi-core runtime instead of single-core runtime. The multi-core
    runtime is generally a bit slower in my experience, but when writing code that actually uses
    multiple cores (such as a test framework that runs tests in parallel) it can give a good
    performance boost
  - [`-rtsopts`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/phases.html#ghc-flag--rtsopts[=%E2%9F%A8none|some|all|ignore|ignoreAll%E2%9F%A9]) -
    Let us configure the Haskell runtime system by passing command-line arguments to our application
  - [`-with-rtsopts=-N`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/phases.html#ghc-flag--with-rtsopts=%E2%9F%A8opts%E2%9F%A9) -
    Set specific default options for the program at link-time.
    Specifically, [`-N`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/using-concurrent.html#rts-flag--N%20%E2%9F%A8x%E2%9F%A9)
    Sets the number of cores to use in our program
- [`build-tool-depends`](https://cabal.readthedocs.io/en/stable/cabal-package.html#pkg-field-build-tool-depends) -
  Use a specific executable from a package dependency in aid of building the package.
  In this case, we are using the `hspec-discover` executable from the
  [`hspec-discover`](https://hackage.haskell.org/package/hspec-discover) package, which
  goes over the source directory for the tests, finds all of the `Spec` files,
  and creates an entry point for the program that will run all the tests it discovered


### Hspec discovery

In order for `hspec-discover` to work, we need to add the following
to the "main" file of the test suite, for us this is `test/Spec.hs`:

```hs
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

That's it! `hspec-discover` will automatically define a `main` for us.
Now we can run the tests using `stack test` or `cabal test` (your choice).
Because we haven't defined any tests, our output is:

```sh
Finished in 0.0000 seconds
0 examples, 0 failures
```

When we add new Hspec tests, `hspec-discover` will find and run them automatically
(though we will still need add them to the `other-modules` section in the cabal file).

For `hspec-discover` to identify modules as test modules, the modules must follow
a convention:

1. Their module names must end with `Spec`
2. They must define a value `spec :: Spec` (which describes the test) and export it
   outside of the module (by adding it to the export list of the module, for example)

## Writing tests

Let's write our first test. We'll create a new module to test
markup parsing. We'll call it `MarkupParsingSpec.hs`. We'll need
the following imports as well:

```hs
module MarkupParsingSpec where

import Test.Hspec
import HsBlog.Markup
```

`Hspec` provides us with a monadic interface for describing, composing and
nesting test specifications (`Spec`s).

Using the `describe` function we can
describe a group of tests, using the `it` function we can add a new test,
and using a function like `shouldBe` we can compare two values and make
sure they are equal by using their `Eq` instance.
If they are, the test will pass, and if not, it will fail with a descriptive error.

Let's try it and write a test that obviously fails!

```hs
spec :: Spec
spec = do
  describe "Markup parsing tests" $ do
    it "empty" $
      shouldBe
        (parse "")
        [Heading 1 "bug"]
```

After adding the module to the `other-modules` list in the cabal file:

```hs
  other-modules:
    MarkupParsingSpec
```

And running the tests, we get this output:

```hs
MarkupParsing
  Markup parsing tests
    empty FAILED [1]

Failures:

  test/MarkupParsingSpec.hs:10:7:
  1) MarkupParsing, Markup parsing tests, empty
       expected: [Heading 1 "bug"]
        but got: []

  To rerun use: --match "/MarkupParsing/Markup parsing tests/empty/"

Randomized with seed 763489823

Finished in 0.0004 seconds
1 example, 1 failure
```

The output describes which tests are running in a hierarchy tree (module, group and test),
whether the tests pass or fail, and if they fail, the output and the expected output.

We can fix our test by matching the expected output:

```hs
      shouldBe
        (parse "")
        []
```

Now, running the tests will produce:

```hs
MarkupParsing
  Markup parsing tests
    empty

Finished in 0.0001 seconds
1 example, 0 failures
```

We can add a few more tests:

```hs
    it "paragraph" $
      shouldBe
        (parse "hello world")
        [Paragraph "hello world"]

    it "heading 1" $
      shouldBe
        (parse "* Heading 1")
        [Heading 1 "Heading 1"]

    it "code" $
      shouldBe
        (parse "> main = putStrLn \"hello world!\"")
        [CodeBlock ["main = putStrLn \"hello world!\""]]
```

And run the tests again:

```sh
MarkupParsing
  Markup parsing tests
    Test empty
    paragraph
    heading 1
    code

Finished in 0.0003 seconds
4 examples, 0 failures
```

This is the gist of writing unit tests with Hspec. It's important to note
that we can nest `Spec`s that are declared with `describe` to create trees,
and of course refactor and move things to different functions and modules
to make our test suite better organized.

For example, we can write our tests like this:

```hs
spec :: Spec
spec = do
  describe "Markup parsing tests" $ do
    simple

simple :: Spec
simple = do
  describe "simple" $ do
    it "empty" $
      shouldBe
        (parse "")
        []

    it "paragraph" $
      shouldBe
        (parse "hello world")
        [Paragraph "hello world"]

    it "heading 1" $
      shouldBe
        (parse "* Heading 1")
        [Heading 1 "Heading 1"]

    it "code" $
      shouldBe
        (parse "> main = putStrLn \"hello world!\"")
        [CodeBlock ["main = putStrLn \"hello world!\""]]
```

Also, there are other "expectations" like `shouldBe` that we can use when writing tests.
They are described in the [Hspec tutorial](https://hspec.github.io/expectations.html)
and can be found in the
[haddock documentation](https://hackage.haskell.org/package/hspec-expectations-0.8.2/docs/Test-Hspec-Expectations.html) as well.

### Raw strings

If we want to write multi-line strings, or avoid escaping strings like we did in the "code"
test, we can use a library called
[raw-strings-qq](https://hackage.haskell.org/package/raw-strings-qq)
which uses a language extension called
[`QuasiQuotes`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/template_haskell.html#extension-QuasiQuotes).
`QuasiQuotes` is a meta-programming extension that provides a mechanism for extending the
syntax of Haskell.

A quasi-quote has the form `[quoter| string |]`, where the quoter is the name
of the function providing the syntax we wish to use, and the string is our input.

In our case, we use the quoter `r`, which is defined in
[raw-strings-qq](https://hackage.haskell.org/package/raw-strings-qq-1.1/docs/Text-RawString-QQ.html),
and write any string we want, with multi-lines and unescaped characters!
We could use this to write the tests
[we previously wrote](04-markup/01-data_type.html#exercises):

```hs
{-# language QuasiQuotes #-}

...

import Text.RawString.QQ

...

example3 :: String
example3 = [r|
Remember that multiple lines with no separation
are grouped together to a single paragraph
but list items remain separate.

# Item 1 of a list
# Item 2 of the same list
|]
```

And add multi-line tests:

```hs
spec :: Spec
spec = do
  describe "Markup parsing tests" $ do
    simple
    multiline


multiline :: Spec
multiline = do
  describe "Multi-line tests" $ do
    it "example3" $
      shouldBe
        (parse example3)
        example3Result


example3 :: String
example3 = [r|
Remember that multiple lines with no separation
are grouped together to a single paragraph
but list items remain separate.

# Item 1 of a list
# Item 2 of the same list
|]

example3Result :: Document
example3Result =
  [ Paragraph "Remember that multiple lines with no separation are grouped together to a single paragraph but list items remain separate."
  , OrderedList
    [ "Item 1 of a list"
    , "Item 2 of the same list"
    ]
  ]
```

Running the tests:

```hs
MarkupParsing
  Markup parsing tests
    simple
      Test empty
      paragraph
      heading 1
      code
    Multi-line tests
      example3

Finished in 0.0004 seconds
5 examples, 0 failures
```

---

**Exercise**: Add a test for the fourth example described in the
[previous exercises](04-markup/01-data_type.html#exercises).


<details><summary>Solution</summary>

```hs
multiline :: Spec
multiline = do
  describe "Multi-line tests" $ do
    it "example3" $
      shouldBe
        (parse example3)
        example3Result

    it "example4" $
      shouldBe
        (parse example4)
        example4Result


example4 :: String
example4 = [r|
* Compiling programs with ghc

Running ghc invokes the Glasgow Haskell Compiler (GHC),
and can be used to compile Haskell modules and programs into native
executables and libraries.

Create a new Haskell source file named hello.hs, and write
the following code in it:

> main = putStrLn "Hello, Haskell!"

Now, we can compile the program by invoking ghc with the file name:

> ➜ ghc hello.hs
> [1 of 1] Compiling Main             ( hello.hs, hello.o )
> Linking hello ...

GHC created the following files:

- hello.hi - Haskell interface file
- hello.o - Object file, the output of the compiler before linking
- hello (or hello.exe on Microsoft Windows) - A native runnable executable.

GHC will produce an executable when the source file satisfies both conditions:

# Defines the main function in the source file
# Defines the module name to be Main, or does not have a module declaration

Otherwise, it will only produce the .o and .hi files.
|]

example4Result :: Document
example4Result =
  [ Heading 1 "Compiling programs with ghc"
  , Paragraph "Running ghc invokes the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell modules and programs into native executables and libraries."
  , Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:"
  , CodeBlock
    [ "main = putStrLn \"Hello, Haskell!\""
    ]
  , Paragraph "Now, we can compile the program by invoking ghc with the file name:"
  , CodeBlock
    [ "➜ ghc hello.hs"
    , "[1 of 1] Compiling Main             ( hello.hs, hello.o )"
    , "Linking hello ..."
    ]
  , Paragraph "GHC created the following files:"
  , UnorderedList
    [ "hello.hi - Haskell interface file"
    , "hello.o - Object file, the output of the compiler before linking"
    , "hello (or hello.exe on Microsoft Windows) - A native runnable executable."
    ]
  , Paragraph "GHC will produce an executable when the source file satisfies both conditions:"
  , OrderedList
    [ "Defines the main function in the source file"
    , "Defines the module name to be Main, or does not have a module declaration"
    ]
  , Paragraph "Otherwise, it will only produce the .o and .hi files."
  ]
```

</details>

---

## Parallel test execution

Without further configuration, Hspec will run all
of our tests on the main thread, sequentially.

There are a couple of ways to configure tests to run
in parallel. One is to manually mark a `Spec`
as parallel by passing it to the `parallel` function,
and another is by creating a /hook/ that will apply
`parallel` to each `Spec` automatically with
`hspec-discover`.

Consult the [Hspec manual](https://hspec.github.io/parallel-spec-execution.html#running-all-tests-in-parallel-with-hspec-discover)
on this topic and try both methods. Remember that
we already enabled the threaded runtime and set it to
use multiple cores in the cabal file.

## Summary

This chapter has been just the tip of the iceberg of the Haskell testing landscape.
We haven't talked about
[property testing](https://www.scs.stanford.edu/16wi-cs240h/slides/testing.html) or
[golden testing](https://ro-che.info/articles/2017-12-04-golden-tests),
testing expected failures, testing IO code, inspection testing, benchmarking, and more.
There's just too much to cover!

My hope is that this chapter
provided you with the basics of how to start writing tests for your own projects.
Please consult the tutorial for your chosen testing framework, and read more about
this very important subject on your own.

> You can view the git commit of
> [the changes we've made](https://github.com/soupi/learn-haskell-blog-generator/commit/da1615b6e0a2a4ff2728528240d790754853bf02)
> and the [code up until now](https://github.com/soupi/learn-haskell-blog-generator/tree/da1615b6e0a2a4ff2728528240d790754853bf02).
