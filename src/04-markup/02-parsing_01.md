# Parsing markup part 01 (Recursion)

Let's have a look at how to parse a multi-lined string of markup text
written by a user and convert it to the `Document` type we defined
in the previous chapter.

Our strategy is to take the string of markup text, and:

1. Split it to a list where each element represents a separate line, and
2. Go over the list line by line and process it, remembering
   information from previous lines if necessary

So the first thing we want to do is to process the string line by line.
We can do that by converting the string to a list of string.
Fortunately the Haskell
[`Prelude`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Prelude.html#v:lines)
module from the Haskell standard library
[`base`](https://hackage.haskell.org/package/base) exposes the function
[`lines`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Prelude.html#v:lines)
that does exactly what we want. The `Prelude` module is exposed in every
Haskell file by default so we don't need to import it.

For the line processing part, let's start by ignoring all of the markup syntax
and just group lines together into paragraphs (paragraphs are separated by an empty line),
and iteratively add new features later in the chapter.

A common solution in imperative programs would be to iterate over the lines
using some _loop_ construct and accumulate lines that should be grouped together
into some intermediate mutable variable. When we reach an empty line, we insert
the content of that variable into another mutable variable that accumulates the
results.

Our approach in Haskell isn't so different, except that we do not use loops
or mutable variables. Instead, we use __recursion__.

## Recursion and accumulating information

Instead of loops, in Haskell we use recursion to model iteration.

Consider the following contrived example: let's say that
we want to write an algorithm for adding two natural numbers together,
and we don't have a standard operation to do that (+), but we do
have two operations we could use on each number: `increment`
and `decrement`.

A solution we could come up with is to slowly "pass" one number
to the other number iteratively, by incrementing one, and decrementing the other.
And we do that until the number we decrement reaches 0.

For example for `3` and `2`:

- We start with `3` and `2`, and we increment `3` and decrement `2`
- On the next step we now have `4` and `1`, we increment `4` and decrement `1`
- On the next step we now have `5` and `0`, since the second number is `0` we declare `5` as the result.

This can be written imperatively using a loop:

```js
function add(n, m) {
  while (m /= 0) {
    n = increment(n);
    m = decrement(m);
  }
  return n;
}
```

We can write the same algorithm in Haskell without mutation using recursion:

```hs
add n m =
  if m /= 0
    then add (increment n) (decrement m)
    else n
```

In Haskell, in order to _emulate iteration with mutable state_, we call the function again
with the values we want the variables to have in the next iteration.

### Evaluation of recursion

Recursion commonly has a bad reputation for being slow and possibly unsafe compared to loops.
This is because in imperative languages, calling a function often requires creating
a new call stack.

However, functional languages (and Haskell in particular) play by different
rules and implement a feature called tail call elimination - when the result of a function call
is the result of the function (this is called tail position), we can just drop the current
stack frame and then allocate one for the function we call, so we don't require `N` stack frames
for `N` iterations.

This is of course only one way to do tail call elimination and other
strategies exist, such as translating code like our recursive `add` above to the iteration version.

#### Laziness

Haskell plays by slightly different rules because it uses a *lazy evaluation strategy*
instead of the much more common strict evaluation strategy. An *evaluation strategy*
refers to "when do we evaluate a computation". In a strict language the answer is simple:
_we evaluate the arguments of a function before entering a function_.

So for example the evaluation of `add (increment 3) (decrement 2)` using strict evaluation
will look like this:

1. Evaluate `increment 3` to `4`
2. Evaluate `decrement 2` to `1`
3. Evaluate `add 4 1`

Or, alternatively (depending on the language) we reverse (1) and (2) and evaluate the arguments
from right-to-left instead of left-to-right.

On the other hand, with lazy evaluation, we *only evaluate computation when we need it*, which
is when it is part of a computation that will have some effect on the
outside world, for example when writing a computation to standard output or sending it over the network.

So unless this computation is required, it won't be evaluated. For example:

```hs
main =
  if add (increment 2) (decrement 3) == 5
    then putStrLn "Yes."
    else putStrLn "No."
```

In the case above, we need the result of `add (increment 2) (decrement 3)`
in order to know which message to write,
so it will be evaluated. But:

```hs
main =
  let
    five = add (increment 2) (decrement 3)
  in
    putStrLn "Not required"
```

In the case above we don't actually need `five`, so we don't evaluate it!

But then if we know we need `add (increment 2) (decrement 3)`,
do we use strict evaluation now? The answer is no - because we might not need
to evaluate the arguments to complete the computation. For example in this case:

```hs
const a b = a

main =
  if const (increment 2) (decrement 3) == 3
    then putStrLn "Yes."
    else putStrLn "No."
```

`const` ignores the second argument and returns the first, so we don't actually need
to calculate `decrement 3` in order to provide an answer to the computation and in
turn output an answer to the screen.

With the lazy evaluation strategy we will evaluate expressions when we need to (when they are required
in order to do something for the user), and we evaluate from the outside in - first
we enter functions, and then we evaluate the arguments when we need to (usually when the thing
we want to evaluate appears in some control flow such as the condition of an `if` expression
or a pattern in pattern matching).

---

I've written a more in-depth blog post about how this works in Haskell:
[Substitution and Equational Reasoning](https://gilmi.me/blog/post/2020/10/01/substitution-and-equational-reasoning).

Please read it and try to evaluate the following program by hand:

```hs
import Prelude hiding (const) -- feel free to ignore this line

increment n = n + 1

decrement n = n - 1

const a b = a

add n m =
  if m /= 0
    then add (increment n) (decrement m)
    else n

main =
  if const (add 3 2) (decrement 3) == 5
    then putStrLn "Yes."
    else putStrLn "No."
```

Remember that evaluation always begins from `main`.

<details>
  <summary>Solution</summary>

evaluating `main`

```hs
if const (add 3 2) (decrement 3) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

expanding `const`

```hs
if add 3 2 == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

expanding `add`

```hs
if (if 2 /= 0 then add (increment 3) (decrement 2) else 3) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

evaluating the control flow `2 /= 0`


```hs
if (if True then add (increment 3) (decrement 2) else 3) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

Choosing the `then` branch

```hs
if (add (increment 3) (decrement 2)) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

expanding `add`

```hs
if
  ( if decrement 2 /= 0
    then add
      (increment (increment 3))
      (decrement (decrement 2))
    else (increment 3)
  ) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

Evaluating `decrement 2` in the control flow (notice how both places change!)

```hs
if
  ( if 1 /= 0
    then add
      (increment (increment 3))
      (decrement 1)
    else (increment 3)
  ) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

Evaluating the control flow `1 /= 0`

```hs
if
  ( if True
    then add
      (increment (increment 3))
      (decrement 1)
    else (increment 3)
  ) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

Choosing the `then` branch

```hs
if
  ( add
    (increment (increment 3))
    (decrement 1)
  ) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

Expanding `add`

```hs
if
  ( if decrement 1 /= 0
    then add
      (increment (increment (increment 3)))
      (decrement (decrement 1))
    else increment (increment 3)
  ) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

Evaluating control flow `decrement 1`

```hs
if
  ( if 0 /= 0
    then add
      (increment (increment (increment 3)))
      (decrement 0)
    else increment (increment 3)
  ) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

Evaluating control flow `0 /= 0`

```hs
if
  ( if False
    then add
      (increment (increment (increment 3)))
      (decrement 0)
    else increment (increment 3)
  ) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

Choosing the `else` branch

```hs
if
  (increment (increment 3)) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

Evaluate control flow `increment (increment 3)`

```hs
if
  (increment 3 + 1) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

Evaluate in control flow `increment 3`

```hs
if
  (3 + 1 + 1) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

Evaluate in control flow `3 + 1`

```hs
if
  (4 + 1) == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

Evaluate in control flow `4 + 1`

```hs
if
  5 == 5
  then putStrLn "Yes."
  else putStrLn "No."
```

Evaluate in control flow `5 == 5`

```hs
if
  True
  then putStrLn "Yes."
  else putStrLn "No."
```

Choosing the `then` branch

```hs
putStrLn "Yes."
```

Which when run will print `Yes.` to the screen.

</details>

---

### General recursion

In general, when trying to solve problems recursively, it is useful to think
about the problem in three parts:

1. Finding the **base case** (the most simple cases - the ones we already know how to answer)
2. Figuring out how to **reduce** the problem to something simpler (so it gets closer to the base case)
3. **Mitigating the difference** between the reduced version and the solution we need to provide

The reduce and mitigate steps together are usually called the *recursive step*.

Let's take a look at another example problem: generating a list of a particular size
with a specific value in place of every element.

In Haskell, this function would have the following signature:

```hs
replicate :: Int -> a -> [a]
```

Here are a few usage examples of `replicate`:

```hs
ghci> replicate 4 True
[True,True,True,True]
ghci> replicate 0 True
[]
ghci> replicate (-13) True
[]
```

How would we implement this function recursively? How would describe it in three steps above?

1. **Base case**: the cases we already know how to generate are the cases where the length
   of the list is zero (or less) - we just return an empty list.
2. **Reduce**: while we might not know how to generate a list of size `N` (where `N` is positive),
   if we knew the solution for `N-1` we could:
3. **Mitigate**: Add another element to the solution for `N-1` using the `:` (cons) operator.

---

Try to write this in Haskell!

<details>
<summary>Solution</summary>

```hs
replicate :: Int -> a -> [a]
replicate n x =
  if n <= 0    -- recognizing the base case
    then
      []       -- the solution for the base case
    else
        x : replicate (n - 1) x
  --   ---  -------------------
  --    ^           ^
  --    |           |
  --    |           +-------- reduction
  --    |
  --    +--- mitigation
```

</details>

---

### Mutual recursion

When solving functions recursively we usually call the same function again,
but that doesn't have to be the case. It is possible to reduce our problem
to something simpler that requires an answer from a different function.
If, in turn, that function will (or another function in that call chain)
call our function again, we have a **mutual recursive** solution.

For example, let's write two functions, one that checks whether a natural number
is even or not, and one that checks whether a number is odd or not
only by decrementing it.

```hs
even :: Int -> Bool

odd :: Int -> Bool
```

Let's start with `even`, how should we solve this recursively?

1. **Base case**: We know the answer for `0` - it is `True`.
2. **Reduction**: We might not know the answer for a general `N`, but we could check whether `N - 1` is odd,
3. **Mitigation**: if `N - 1` is odd, then `N` is even! if it isn't odd, then `N` isn't even.

What about `odd`?

1. **Base case**: We know the answer for `0` - it is `False`.
2. **Reduction**: We might not know the answer for a general `N`, but we could check whether `N - 1` is even,
3. **Mitigation**: if `N - 1` is even, then `N` is odd! if it isn't even, then `N` isn't odd.

---

Try writing this in Haskell!

<details>
<summary>Solution</summary>

```hs
even :: Int -> Bool
even n =
  if n == 0
    then
      True
    else
      odd (n - 1)

odd :: Int -> Bool
odd n =
  if n == 0
    then
      False
    else
      even (n - 1)

```

</details>

---

## Partial functions

Because we didn't handle the negative numbers cases in the example above,
our functions will loop forever when a negative value is passed as input.
A function that does not return a result for some value
(either by not terminating or by throwing an error) is called **a partial function**
(because it only returns a result for a part of the possible inputs).

Partial functions are generally considered **bad practice** because they can have
undesired behaviour at runtime (a runtime exception or an infinite loop),
so we want to **avoid using** partial functions
as well as **avoid writing** partial functions.

The best way to avoid writing partial functions is by covering all inputs!
In the situation above, it is definitely possible to handle negative numbers
as well, so we should do that! Or, instead, we could require that our functions
accept a `Natural` instead of an `Int`, and then the type system would've stopped
us from using these functions with values that we did not handle.

There are cases where we can't possibly cover all inputs, in these cases it is important
to re-examine the code and see if we could further restrict the inputs using types to
mitigate these issues.

For example, the `head :: [a] -> a` function from `Prelude` promises
to return the first element (the head) of a list, but we know that lists
could possibly be empty, so how can this function deliver on its promise?

Unfortunately, it can't. But there exists a different function that can:
`head :: NonEmpty a -> a` from the
[`Data.List.NonEmpty`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Data-List-NonEmpty.html)
module! The trick here is that this other `head` does not take a general list
as input, it takes a different type entirely, one that promises to have
at least one element, and therefore can deliver on its promise!

We could also potentially use smart constructors with `newtype` and enforce some sort
of restrictions in the type system, as we saw in earlier chapters,
But this solution can sometimes be less ergonomic to use.

An alternative approach is to use `data` types to encode the absence of a proper result,
for example, using `Maybe`, as we'll see in a future chapter.

Make sure the functions you write return a result for every input,
either by constraining the input using types, or by encoding the absence of a result using
types.

## Parsing markup?

Let's get back to the task at hand.

As stated previously, our strategy for parsing the markup text is:

1. Split the string to a list where each element is a separate line
   (which we can do with [`lines`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Prelude.html#v:lines)), and
2. Go over the list line by line and process it, remembering
   information from previous lines if necessary

Remember that we want to start by ignoring all of the markup syntax
and just group lines together into paragraphs (paragraphs are separated by an empty line),
and iteratively add new features later in the chapter:

```hs
parse :: String -> Document
parse = parseLines [] . lines -- (1)

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
  let
    paragraph = Paragraph (unlines (reverse currentParagraph)) -- (2), (3)
  in
    case txts of -- (4)
      [] -> [paragraph]
      currentLine : rest ->
        if trim currentLine == ""
          then
            paragraph : parseLines [] rest -- (5)
          else
            parseLines (currentLine : currentParagraph) rest -- (6)

trim :: String -> String
trim = unwords . words
```

Things to note:

1. We pass a list that contains the currently grouped paragraph (paragraphs are separated by an empty line)
2. Because of laziness, `paragraph` is not computed until it's needed, so we don't have to worry about
  the performance implications in the case that we are still grouping lines
3. Why do we reverse `currentParagraph`? (See point (6))
4. We saw case expressions used to deconstruct `newtype`s and `Char`s,
   but we can also pattern match on lists and other ADTs as well!
   In this case we match against two patterns, an empty list (`[]`),
   and a "cons cell" - a list with at least one element (`currentLine : rest`).
   In the body of the "cons" pattern, we bind the first element to the name `currentLine`,
   and the rest of the elements to the name `rest`.

   We will talk about how all of this works really soon!
5. When we run into an empty line we add the accumulated paragraph to the resulting list (A `Document` is a list of structures) and start the function again with the rest of the input.
6. We pass the new lines to be grouped in a paragraph **in reverse order** because of
   performance characteristics - because of the nature of singly-linked lists,
   prepending an element is fast, and appending is slow. Prepending only requires
   us to create a new cons (`:`) cell to hold a pointer to the value and a pointer to the list,
   but appending requires us to traverse the list to its end and rebuild the cons cells -
   the last one will contain the last value of the list and a pointer to the list to append,
   the next will contain the value before the last value of the list and a pointer to the
   list which contains the last element and the appended list, and so on.


This code above will group together paragraphs in a structure, but how do we view our result?
In the next chapter we will take a short detour and talk about type classes, and how
they can help us in this scenario.
