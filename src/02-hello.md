# Hello, world!

In this chapter we will create a simple HTML hello world and use the Haskell tool-chain
to run it.

> If you haven't installed a Haskell tool-chain yet, visit
> [haskell.org/downloads](https://haskell.org/downloads) for instructions on how to download
> and install a Haskell tool-chain.

A Haskell source file is composed of definitions.

The most common definition has the following form:

```hs
<name> = <expression>
```

Note that:

1. We cannot write expressions without binding them to a name
2. Names must start with a lowercase letter
3. We cannot use the same name more than once in a file

A source file containing a definition of the name `main` can be treated as an executable,
and the expression `main` is bound to is the entry point to the program.

Let's create a new Haskell source file called `hello.hs`, and write the following line there:

```hs
main = putStrLn "<html><body>Hello, world!</body></html>"
```

We've defined a new name, `main`, and bound it to the expression `putStrLn "<html><body>Hello, world!</body></html>"`.

the body of `main` means calling the function `putStrLn` with the string `"<html><body>Hello, world!</body></html>"`.
`putStrLn` takes a single string as input and prints that string to the standard output.

__Note__: we don't need parenthesis to pass arguments to functions in Haskell.

Running this program will result in the following text printed on the screen:

```
<html><body>Hello, world!</body></html>
```

To run this little program, we can either compile it using the command line program `ghc`, like this: `ghc hello.hs` which will create a few files:

1. `hello.o` - Object file
2. `hello.hi` - Haskell interface file
3. `hello` - A native executable file

and then run `hello`.

Or, alternatively, interpret the source file using the command line program `runghc`,
like this: `runghc hello.hs`. This will run the program without compiling it or creating any files.

```sh
> runghc hello.hs
<html><body>Hello, world!</body></html>
```

We can also redirect the output of the program to a file and then open it in Firefox.

```sh
> runghc hello.hs > hello.html
> firefox hello.html
```

This command should open Firefox and display a web page with `Hello, world!` written in it.

In this tutorial we'll use `runghc` regularly, because it saves us time to run the program without compiling it.

---

__Note__: If you've installed a GHC tool-chain via stack and not GHCup, and the commands above do not work because `ghc` or `runghc` are missing, prefix the commands above with `stack exec -- `. Stack will locate the globally installed GHC and use it. For example `runghc hello.hs` becomes `stack exec -- runghc hello.hs`. See the [Stack user guide](https://docs.haskellstack.org/en/stable/GUIDE/#exec) for more details.

---

> If you want to learn more about the core Haskell tools, [you can read this article](https://gilmi.me/blog/post/2021/08/14/hs-core-tools). But what's described above is enough for now.

## More bindings

We can define the HTML string passed to `putStrLn` in a new name instead of passing
it directly to `putStrLn`. Change the content of file `hello.hs` we defined above to:

```hs
main = putStrLn myhtml

myhtml = "<html><body>Hello, world!</body></html>"
```

__Note__: the order in which we declare the bindings does not matter.

