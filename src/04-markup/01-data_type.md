# Representing the markup language as a Haskell data type

One of the clear differentiators between Haskell (also other ML-family of languages)
and most mainstream languages is the ability to represent data precisely and succinctly.

So how do we represent our markup language using Haskell?

Previously, in our HTML builder library, we used `newtype`s to differentiate
between HTML documents, structures and titles, but we didn't really need to
differentiate between different kinds of structures such as paragraphs and headings,
not without parsing the data at least.

In this case, we have a list of structures, and each structure could be
one of a few specific options (a paragraph, a heading, a list, etc.),
and we want to be able to know which structure is which so we can easily
convert it into the equivalent HTML representation.

For that, we have `data` definitions. `data` gives us the ability to
create custom types by grouping multiple types together and having
alternative structures. Think of them as combination of both structs and enums.

`data` declarations look like this:

```hs
data <Type-name> <type-args>
  = <Data-constructor1> <types>
  | <Data-constructor2> <types>
  | ...
```

It looks really similar to `newtype`, but there are two important
differences:

1. In the `<types>` part we can write many types (Like `Int`, `String`, or `Bool`).
   For `newtype`s we can only write one.
2. We can have alternative structures using `|`, `newtype`s have no
   alternatives.

This is because `newtype` is used to provide a type safe __alias__, and `data`
is used to build a new **composite** type that can potentially have *alternatives*.

Let's see a few of examples of data types:

1. Bool

   ```hs
   data Bool
     = True
     | False
   ```

   We created a new data type named `Bool` with the possible values `True` or `False`.
   In this case we only have *constructor* alternatives and none of the constructors
   carry additional values. This is similar to enums in other languages.

2. Person

   ```hs
   data Person
     = Person String Int -- where the first is the name and the second is
                         -- the age
   ```

   We created a new data type named `Person`. Values of the type `Person`
   look like this:

   ```
   Person <some-string> <some-int>
   ```

   For example:

   ```hs
   Person "Gil" 32
   ```

   In this case we create a *composite* of multiple types, without alternatives.
   This is similar to structs in other language, but structs give each field
   a name, and here we distinguish them by position.

   Alternatively, Haskell has *syntactic sugar* for naming fields called **records**.
   The above definition can also be written like this:


   ```hs
   data Person
     = Person
       { name :: String
       , age :: Int
       }
   ```

   Values of this type can be written exactly as before,

   ```hs
   Person "Gil" 32
   ```

   Or with this syntax:

   ```hs
   Person { name = "Gil", age = 32 }
   ```

   Haskell will also generate functions that can be used to extract the fields from the composite type:

   ```hs
   name :: Person -> String
   age :: Person -> Int
   ```

   Which can be used like this:

   ```hs
   ghci> age (Person { name = "Gil", age = 32 })
   32
   ```

   We even have special syntax for updating specific fields in a record. Of course,
   we do not update records in place - we generate a new value instead.

   ```hs
   ghci> gil = Person { name = "Gil", age = 32 }
   ghci> age (gil { age = 33 })
   33
   ghci> age gil
   32
   ```

   Unfortunately, having specialized functions for each field also means that if we
   defined a different data type with the field `age`, the functions which GHC needs
   to generate will clash.

   The easiest way to solve this is to give fields unique names, for example
   by adding a prefix:

   ```hs
   data Person
     = Person
       { pName :: String
       , pAge :: Int
       }
   ```

   Another way is by using extensions to the Haskell language, which we will cover
   in later chapters.

3. Tuple

   ```hs
   data Tuple a b
     = Tuple a b
   ```

   This is pretty similar to `Person`, but we can plug any type we want
   for this definition. For example:

   ```hs
   Tuple "Clicked" True :: Tuple String Bool

   Tuple 'a' 'z' :: Tuple Char Char
   ```

   This type has special syntax in Haskell:

   ```hs
   ("Clicked", True) :: (String, Bool)

   ('a', 'z') :: (Char, Char)
   ```

    This `Tuple` definition is polymorphic, we define the structure but are able to
    plug different types into the structure to get concrete types. You can think of `Tuple`
    as a *template* for a data type waiting to be filled, or as a **function** waiting
    for types as input in order to return a data type. We can even take a look at the "type"
    signature of `Tuple` in `ghci` using the `:kind` command.

    ```hs
    ghci> data Tuple a b = Tuple a b
    ghci> :kind Tuple
    Tuple :: * -> * -> *
    ```

    > #### Quick detour: Kinds
    >
    > The `:kind` command is called as such because the "type" of a type is called a **kind**.
    > Kinds can be one of two things, either a `*` which means a saturated (or concrete) type,
    > such as `Int` or `Person`, or an `->` of two kinds, which is, as you might have guessed,
    > a type function, taking kind and returning a kind.
    >
    > Note that only types that have the kind `*` can have values. So for example while `Tuple Int`
    > is a valid Haskell concept that has the *kind* `* -> *`, and we can write code that will
    > work "generically" for all types that have a certain kind (e.g. `* -> *`), we cannot
    > construct a value that will have the kind `* -> *`. All values have types, and all
    > types that have values have the kind `*`.
    >
    > We will talk more about kinds later, for now let's focus on types!

4. Either

   ```hs
   data Either a b
     = Left a
     | Right b
   ```

   Similar to Tuple but instead of having only one constructor, we have
   two. This means that we can choose which side we want. Here are a
   couple of values of type `Either String Int`:

   ```hs
   Left "Hello"

   Right 17
   ```

   This type is useful for modeling errors. Either we succeeded and got
   what we wanted (The `Right` constructor with the value), or we didn't
   and got an error instead (The `Left` constructor with a string or a
   custom error type).

In our program we use `data` types to model the different kinds of content types
in our markup language. We tag each structure using the data constructor
and provide the rest of the information (the paragraph text, the list items, etc.)
in the `<types>` section of the data declaration for each constructor:

```hs
type Document
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
```

Note: `Natural` is defined in the `base` package but not exported from `Prelude`.
Find out which module to import `Natural` by using [Hoogle](https://hoogle.haskell.org).

---

### Exercises

Represent the following markup documents as values of `Document`:

1. ```org
   Hello, world!
   ```

2. ```org
   * Welcome

   To this tutorial about Haskell.
   ```

3. ```org
   Remember that multiple lines with no separation
   are grouped together to a single paragraph
   but list items remain separate.

   # Item 1 of a list
   # Item 2 of the same list
   ```

4. ```org
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
   ```

Solutions:

<details>
  <summary>Solution 1</summary>

```hs
example1 :: Document
example1 =
  [ Paragraph "Hello, world!"
  ]
```

</details>

<details>
  <summary>Solution 2</summary>

```hs
example2 :: Document
example2 =
  [ Heading 1 "Welcome"
  , Paragraph "To this tutorial about Haskell."
  ]
```

</details>

<details>
  <summary>Solution 3</summary>

```hs
example3 :: Document
example3 =
  [ Paragraph "Remember that multiple lines with no separation are grouped together to a single paragraph but list items remain separate."
  , OrderedList
    [ "Item 1 of a list"
    , "Item 2 of the same list"
    ]
  ]
```

</details>

<details>
  <summary>Solution 4</summary>

```hs
example4 :: Document
example4 =
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

Add a new module named `Markup` and add the data type definition to it.
Note that in this case we *do* want to export the constructors of `Structure`.

<details>
  <summary>Solution</summary>

```hs
-- Markup.hs

module Markup
  ( Document
  , Structure(..)
  )
where

import Numeric.Natural

type Document
  = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
```

</details>

---

## Translating directly?

You might ask "Why do we even need to represent the markup as a type?
Why don't we convert it into HTML as soon as we parse it
instead?". That's a good question and a valid strategy. The reason we
first represent it as a Haskell type is for flexibility and modularity.

If the parsing code is coupled with HTML generation, we lose the
ability to pre-process the markup document. For example we might want
to take only a small part of the document (for summary) and present
it, or create a table of content from headings. Or maybe we'd like to
add other targets and not just HTML - maybe markdown format or a GUI reader?

Parsing to an "abstract data type" (ADT) representation (one that does
not contain the details of the language, for example '#' for
ordered lists) gives us the freedom to do so much more than just
conversion to HTML that it's usually worth it in my opinion unless you
really need to optimize the process.
