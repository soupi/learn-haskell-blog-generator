# Exercises

We need a few more features for our HTML library to be useful for
our blog software. Add the following features to our `Html.Internal` module
and expose them from `Html`.

## 1. Unordered lists

These lists have the form:

```html
<ul>
  <li>item 1</li>
  <li>item 2</li>
  <li>...</li>
</ul>
```

 We want in our library a new function:
```hs
ul_ :: [Structure] -> Structure
```

So that users can write this:

```hs
ul_
  [ p_ "item 1"
  , p_ "item 2"
  , p_ "item 3"
  ]
 ```

and get this:

```html
<ul>
  <li><p>item 1</p></li>
  <li><p>item 2</p></li>
  <li><p>item 3</p></li>
</ul>
```

## 2. Ordered lists

Very similar to unordered lists, but instead of `<ul>` we use `<ol>`

## 3. Code blocks

Very similar to `<p>`, but use the `<pre>` tag. Call this function `code_`.


## Solutions

<details>
  <summary>Unordered lists</summary>

```hs
ul_ :: [Structure] -> Structure
ul_ =
  Structure . el "ul" . concat . map (el "li" . getStructureString)
```

</details>


<details>
  <summary>Ordered lists</summary>

```hs
ol_ :: [Structure] -> Structure
ol_ =
  Structure . el "ol" . concat . map (el "li" . getStructureString)
```

Note: the two functions above could be unified.

</details>


<details>
  <summary>Code blocks</summary>

```hs
code_ :: String -> Structure
code_ = Structure . el "pre" . escape
```

</details>

