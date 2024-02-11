# About this book

> <p style="text-align: center;"> Looking for reviews and mentions? <a href="https://github.com/soupi/learn-haskell-blog-generator/discussions/67">Click here</a>.</p>

<!--

> <p style="text-align: center;"><img src="book-logo-transparent.png" alt="book logo" style="max-height: 1.5em; vertical-align: top"> This book is actively maintained. If you find errors, <a href="https://github.com/soupi/learn-haskell-blog-generator/issues">please let me know</a>.</p>

-->

<!--
<div style="text-align: center">
  <img src="book-logo-transparent.png" alt="book logo" style="max-width: 40%">
</div>
-->

In this book, we will implement a simple static blog generator in Haskell,
converting documents written in our own custom markup language to HTML.

We will:

1. Implement a tiny HTML printer library
2. Define and parse our own custom markup language
3. Read files and glue things together
4. Add command line arguments parsing
5. Write tests and documentation

In each chapter of the book, we will focus on a particular task we wish to achieve,
and throughout the chapter, learn just enough Haskell to complete the task.

## Other ways to read this book

<div style="display: flex; flex-wrap: wrap; align-items: center; justify-content: center;">

<div style="text-align: center; margin: 5px">
  <a href="https://www.youtube.com/watch?v=ZL0qExCnO8g&list=PLxn_Aq3QlOQcXoHWdzxnnuGlGWNXJg43R&index=1" title="Learn Haskell by building a blog video series by Impure Pics">
    <img style="max-height: 140px;" src="https://i.ytimg.com/vi/ZL0qExCnO8g/hqdefault.jpg?sqp=-oaymwEXCNACELwBSFryq4qpAwkIARUAAIhCGAE=&rs=AOn4CLDQbKeP3DE3OL0JN1oL8FYWyQ85JA" border=1>
  </a>
  <p style="width: 260px; margin: 0px;">Do you prefer watching videos?<br>
  <a href="https://impurepics.com">Impure Pics</a> made a video series based on this book!
  </p>
</div>

<div style="text-align: center; margin: 5px">
  <a href="lhbg-v0.pdf" title="Experimental v0 PDF version of this book">
    <img style="max-height: 140px;" src="pdf.png" border=1>
  </a>
  <p style="width: 260px; margin: 0px;">Do you prefer reading PDFs?<br>
  An experimental PDF version is now available.</p>
</div>

</div>

## Why should you read this book?

There are many Haskell tutorials, guides, and books out there. Why read this one?

### Pros

There are probably more, but here are a few possible pros:

- It is **relatively short** - most Haskell books are hundreds of pages long.
  This book (when exported to PDF) is roughly 150 pages long.
- It is **project oriented**. Many Haskell books teach Haskell by teaching the underlying
  concepts and features in a neat progression. In this book, we **build a Haskell program**
  and learn Haskell on the way. This will be a pro to some and a con to others.<br>
  There are other tutorials like this. The most notable ones are
  [Beginning Haskell](https://www.apress.com/gp/book/9781430262510#otherversion=9781430262503)
  and [Haskell via Sokoban](https://haskell-via-sokoban.nomeata.de/).
- It touches on **important topics** such as design patterns, testing, and documentation.
- It has **many exercises** as well as **solutions** to those exercises.
- It's **online**, which means corrections are easy to make.
- It's **free**.

### Cons

There are probably more, but here are a few possible cons:

- It **may lack depth** - many, much longer Haskell tutorials are long because they go
  deeper into the nuts and bolts of each feature, and I tried to keep this book relatively short.
- It **may not cover as many features or techniques** as other tutorials -
  we try to cover features as they pop up in our implementation, but we will
  probably miss features that aren't as important for our tasks,
  while other resources may try to cover many different use cases.
- It **does not have a technical editor**, though it has seen quite a bit of editing.

### Other learning resources

The [haskell.org/documentation](https://www.haskell.org/documentation/) page lists
many tutorials, books, guides, and courses. You can find a few alternatives that I can
recommend [in this list](https://github.com/soupi/haskell-study-plan#about-this-guide).

<!--

### Who am I?

I'm
[<img src="https://avatars.githubusercontent.com/u/8547573" alt="🐨" style="border-radius: 100px; max-height: 1.5em; vertical-align: top">
gilmi](https://gilmi.me).

-->

## Discussions

Do you want to discuss the book? Maybe ask a question?
Try the [discussion board](https://github.com/soupi/learn-haskell-blog-generator/discussions)!

