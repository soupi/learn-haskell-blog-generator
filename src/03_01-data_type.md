# Representing the markup language as a Haskell data type

One of the clear differentiators between Haskell and other ML-family of languages
from most mainstream languages is the ability to represent data precisely and succinctly.

So how do we represent our markup language using Haskell?

Previously, in our HTML builder library, we used `newtype`s to differentiate
between html documents, structures and titles, but we didn't really need to
differentiate between different kinds of structures such as paragraphs and headers,
not without parsing the data at least.

In this case, we have a list of structures, and each structure could be
one of a few specific options (a paragraph, a header, a list, etc),
and we want to be able to know which structure is which so we can easily
convert it into the equivalent HTML representation.

For that, we have `data` definitions. `data` gives us the ability to
create custom types by grouping multiple types together and having
alternative structures. Think of them as combination of both structs and enums.

`data` declarations look like this:

```
data <Type-name> <type-args>
  = <Data-constructor1> <types>
  | <Data-constructor2> <types>
  | ...
```

It looks really similar to `newtype`, but there are two important
differences:

1. In the <types> part we can write many types (Like `Int String Bool`).
   For `newtype`s we can only write one.
2. We can have alternative structures using `|`, `newtype`s have no
   alternatives.

Let's see a couple of examples of data types:

1. Bool

```
data Bool
  = True
  | False
```

We created a new data type named `Bool` with the possible values `True` or `False`.

2. Person

```
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

```
Person "Gil" 32
```

3. Tuple

```
data Tuple a b
  = Tuple a b
```

This is pretty similar to `Person`, but we can plug any type we want
for this definition. For example:

```
Tuple "Clicked" True :: Tuple String Bool

Tuple 'a' 'z' :: Tuple Char Char
```

This type has special syntax in Haskell:

```
("Clicked", True) :: (String, Bool)

('a', 'z') :: (Char, Char)
```

4. Either

```
data Either a b
  = Left a
  | Right b
```

Similar to Tuple but instead of having only one constructor, we have
two. This means that we can choose which side we want. Here are a
couple of Values of type `Either String Int`:

```
Left "Hello"

Right 17
```

This type is useful for modeling errors. Either we succeeded and got
what we wanted (The `Right` constructor with the value), or we didn't
and got an error instead (The `Left` constructor with a string or a
custom error type).

Here we use it to model the different kinds of content types we have
in our markup language. We tag each structure using the data constructor
and provide the rest of the information (the paragraph text, the list items, etc)
in the `<types>` section of the data declaration for each constructor:

```hs
type Document
  = [Structure]

data Structure
  = Header Int String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
```

---

Exercises:

Represent the following markup documents as value of `Document`:

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
doc :: Document
doc =
  [ Paragraph "Hello, world!"
  ]
```

</details>

<details>
  <summary>Solution 2</summary>

```hs
doc :: Document
doc =
  [ Header 1 "Welcome"
  , Paragraph "To this tutorial about Haskell."
  ]
```

</details>

<details>
  <summary>Solution 3</summary>

```hs
doc :: Document
doc =
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
doc :: Document
doc =
  [ Header 1 "Compiling programs with ghc"
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

type Document
  = [Structure]

data Structure
  = Header Int String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
```

</details>

---

You might ask "Why do we even need to represent the markup as a type?
Why don't we convert it into html as soon as we parse it
instead?". That's a good question and a valid strategy. The reason we
first represent it as a Haskell type is for flexibility and
modularity.

If the parsing code is coupled with html generation, we lose the
ability to preprocess the markup document. For example we might want
to take only a small part of the document (for summary) and present
it, or create a table of content from headers. Or maybe we'd like to
add other targets and not just html - maybe markdown format or a gui reader?

Parsing to an "abstract data type" (ADT) representation (one that does
not contain the details of the language, that for example that we use '#' for
ordered lists) gives us the freedom to do so much more than just
converting to html that it's usually worth it in my opinion unless you
really need to optimize the process.
