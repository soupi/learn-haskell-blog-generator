# Custom markup language

In this chapter we will define our own simple markup language
and parse documents written in this language into Haskell data structures.

Our markup language will contain the following features:

- Headings: prefix by a number of `*` characters
- Paragraphs: a group of lines without empty lines in between
- Unordered lists: a group of lines each prefixed with `- `
- Ordered lists: a group of lines each prefixed with `# `
- Code blocks: a group of lines each prefixed with `> `

Here's a sample document:

```org
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

which we will, eventually, convert into this (modulo formatting) HTML:

```html
<h1>Compiling programs with ghc</h1>

<p>Running ghc invokes the Glasgow Haskell Compiler (GHC),
and can be used to compile Haskell modules and programs into native
executables and libraries.
</p>

<p>Create a new Haskell source file named hello.hs, and write
the following code in it:
</p>

<pre>main = putStrLn "Hello, Haskell!"
</pre>

<p>Now, we can compile the program by invoking ghc with the file name:</p>

<pre>
➜ ghc hello.hs
[1 of 1] Compiling Main             ( hello.hs, hello.o )
Linking hello ...
</pre>

<p>GHC created the following files:
</p>

<ul>
  <li>hello.hi - Haskell interface file</li>
  <li>hello.o - Object file, the output of the compiler before linking</li>
  <li>hello (or hello.exe on Microsoft Windows) - A native runnable executable.</li>
</ul>

<p>GHC will produce an executable when the source file satisfies both conditions:
</p>

<ol>
  <li>Defines the main function in the source file</li>
  <li>Defines the module name to be Main, or does not have a module declaration</li>
</ol>

<p>Otherwise, it will only produce the .o and .hi files.
</p>
```
