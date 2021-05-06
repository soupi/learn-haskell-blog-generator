# Exercises

We need a few more features for our html library to be useful for
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
ul_ :: [HtmlBodyContent] -> HtmlBodyContent
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
  <li>item 1</li>
  <li>item 2</li>
  <li>item 3</li>
</ul>
```

## 2. Ordered lists

Very similar to unordered lists, but instead of `<ul>` we use `<ol>`

## 3. Code blocks

Very similar to `<p>`, but use the `<pre>` tag. Call this function `code_`.


## Solutions

