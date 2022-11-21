# Flexible HTML content (functions)

We'd like to be able to write different HTML pages without having to write the whole
structure of HTML and body tags over and over again. We can do that with functions.

To define a function, we create a definition like we saw previously and add the argument
names after the name and before the equals sign (`=`).
So a function definition has the following form:

```hs
<name> <arg1> <arg2> ... <argN> = <expression>
```

The argument names will be available in scope on the right side of the equals sign
(in the `<expression>`), and the function name will be `<name>`.

We'll define a function that takes a string, which is the content of the page, and wraps it in
the relevant `html` and `body` tags by concatenating them before and after the content.
We use the operator `<>` to concatenate two strings.

```hs
wrapHtml content = "<html><body>" <> content <> "</body></html>"
```

This function, `wrapHtml`, takes one argument named `content` and returns a string
that prefixes `<html><body>` before the content and appends `</body></html>` after it.
Note that it is common to use camelCase in Haskell for names.

Now we can adjust our `myhtml` definition from the previous chapter:

```hs
myhtml = wrapHtml "Hello, world!"
```

Again, notice that we don't need parenthesis when calling functions. Function calls have the form:

```hs
<name> <arg1> <arg2> ... <argN>
```

However, if we wanted to substitute `myhtml` with the expression `myhtml` is bound
to in `main = putStrLn myhtml`, we would have to wrap the expression in parenthesis:

```hs
main = putStrLn (wrapHtml "Hello, world!")
```

If we accidentally write this instead:

```hs
main = putStrLn wrapHtml "Hello, world!"
```


we'll get an error from GHC stating that `putStrLn` is applied to two arguments,
but it only takes one. This is because the above is of the form `<name> <arg1> <arg2>`
in which, as we defined earlier, `<arg1>` and `<arg2>` are arguments to `<name>`.

By using parenthesis we can group together the expressions in the right order.

> #### An aside about operator precedence and fixity
>
> operators (like `<>`) are infix functions which take two arguments - one from each side.
>
> When there are multiple operators in the same expression without parenthesis, the operator
> *fixity* (left or right) and *precedence* (a number between 0 and 10) determine which
> operator binds more tightly.
>
> In our case `<>` has *right* fixity, so Haskell adds invisible parenthesis on the right side
> of `<>`. So for example:
>
> ```hs
> "<html><body>" <> content <> "</body></html>"
> ```
>
> is viewed by Haskell as:
>
> ```hs
> "<html><body>" <> (content <> "</body></html>")
> ```
>
> For an example of precedence, in the expression `1 + 2 * 3`,
> the operator `+` has precedence 6, and the operator `*` has precedence 7,
> so we give precedence to `*` over `+`. Haskell will view this expression as:
>
> ```hs
> 1 + (2 * 3)
> ```
>
> You might run into errors when mixing different operators with the *same precedence*
> but *different fixity*, because Haskell won't understand how to group these expressions.
> In that case we can solve the problem by adding parenthesis explicitly.

---

Exercises:

1. Separate the functionality of `wrapHtml` to two functions:
   1. One that wraps content in `html` tag
   2. one that wraps content in a `body` tag

   Name the new functions `html_` and `body_`.
2. Change `myhtml` to use these two functions.
3. Add another two similar functions for the tags `<head>` and `<title>`
   and name them `head_` and `title_`.
4. Create a new function, `makeHtml`, which takes two strings as input:
   1. One string for the title
   2. One string for the body content
   
   And construct an HTML string using the functions implemented in the previous exercises.
   
   The output for:
   
   ```hs
   makeHtml "My page title" "My page content"
   ```
   
   should be:
   
   ```html
   <html><head><title>My page title</title></head><body>My page content</body></html>
   ```
5. Use `makeHtml` in `myhtml` instead of using `html_` and `body_` directly

---

Solutions:

<details>
  <summary>Solution for exercise #1</summary>
  
  ```hs
  html_ content = "<html>" <> content <> "</html>"
     
  body_ content = "<body>" <> content <> "</body>"
  ```

</details>

<details>
  <summary>Solution for exercise #2</summary>
  
  ```hs
  myhtml = html_ (body_ "Hello, world!")
  ```

</details>

<details>
  <summary>Solution for exercise #3</summary>
  
  ```hs
  head_ content = "<head>" <> content <> "</head>"
  
  title_ content = "<title>" <> content <> "</title>"
  ```

</details>

<details>
  <summary>Solution for exercise #4</summary>
  
  ```hs
  makeHtml title content = html_ (head_ (title_ title) <> body_ content)
  ```

</details>


<details>
  <summary>Solution for exercise #5</summary>
  
  ```hs
  myhtml = makeHtml "Hello title" "Hello, world!"
  ```

</details>


<details>
  <summary>Our final program</summary>
  
  ```hs
  -- hello.hs

  main = putStrLn myhtml

  myhtml = makeHtml "Hello title" "Hello, world!"

  makeHtml title content = html_ (head_ (title_ title) <> body_ content)

  html_ content = "<html>" <> content <> "</html>"
     
  body_ content = "<body>" <> content <> "</body>"

  head_ content = "<head>" <> content <> "</head>"
  
  title_ content = "<title>" <> content <> "</title>"
  ```

   We can now run our `hello.hs` program, pipeline the output into a file,
   and open it in our browser:
   
   ```sh
   runghc hello.hs > hello.html
   firefox hello.html
   ```

It should display `Hello, world!` on the page and `Hello title` on the page's title.

</details>


---

## Indentation

You might ask how does Haskell know a definition is complete?
The answer is: Haskell uses indentation to know when things should be grouped together.

Indentation in Haskell can be a bit tricky, but in general: code which is supposed to be
part of some expression should be indented further than the beginning of that expression.

We know two definitions are separate because the second one is not indented further than the first one.


### Indentation tips

1. Choose a specific amount of spaces for indentation (2 spaces, 4 spaces, etc) and stick to it.
   Always use spaces over tabs.
2. Do not indent more than once in any given time.
3. When in doubt, drop line as needed and indent once.

Here are a few examples:

```hs
main =
    putStrLn "Hello, world!"
```

or:

```hs
main =
    putStrLn
        (wrapHtml "Hello, world!")
```

__Avoid the following styles__, which use more than one indentation steps, or completely disregard
indentation steps:

```hs
main = putStrLn
        (wrapHtml "Hello, world!")
```

```hs
main = putStrLn
                (wrapHtml "Hello, world!")
```

