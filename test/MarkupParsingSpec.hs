-- test/MarkupParsingSpec.hs

{-# language QuasiQuotes #-}

module MarkupParsingSpec where

import Test.Hspec
import Text.RawString.QQ
import HsBlog.Markup

spec :: Spec
spec = do
  describe "Markup parsing tests" $ do
    simple
    multiline

simple :: Spec
simple = do
  describe "simple" $ do
    it "Test empty" $
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
