# About this book

> <img src="book-logo-transparent.png" alt="book logo" style="max-height: 1.5em; vertical-align: top"> This book is actively maintained. If you find errors, [please let me know](https://github.com/soupi/learn-haskell-blog-generator/issues).

<!--
<div style="text-align: center">
  <img src="book-logo-transparent.png" alt="book logo" style="max-width: 40%">
</div>
-->

In this book we will implement a simple static blog generator in Haskell,
converting documents written in our own custom markup language to HTML.

We will:

1. Implement a tiny HTML printer library
2. Define and parse our own custom markup language
3. Read files and glue things together
4. Add command line arguments parsing
5. Write tests and documentation

In each chapter of the book we will focus on a particular task we wish to achieve,
and through the chapter we'll learn just enough Haskell to complete the task.

## Why should you read this book?

[[Mentions of lhbg-book in other places](https://github.com/soupi/learn-haskell-blog-generator/discussions/67)]

There are many Haskell tutorials, guides and books out there. Why read this one?

### Pros

There are probably more, but here are a few possible pros:

- It is **relatively short** - most Haskell books are hundreds of pages long.
  This book (when exported to PDF) is roughly 200 pages long.
- It is **project oriented**. Many Haskell books teach Haskell by teaching the underlying
  concepts and features in a neat progression. In this book we **build a Haskell program**,
  and learn Haskell on the way. This will be a pro to some, and a con to others.<br>
  There are other tutorials like this. The most notable ones are
  [Beginning Haskell](https://www.apress.com/gp/book/9781430262510#otherversion=9781430262503)
  and [Haskell via Sokoban](https://haskell-via-sokoban.nomeata.de/).
- It touches on **important topics** such as design patterns, testing and documentation.
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
- It is **relatively new** and not "battle-tested". Who knows if this is a good approach to
  learning Haskell? Maybe you could help with that and let me know!
- It **does not have a technical editor**, though it has seen quite a bit of editing.

### Other learning resources

The [haskell.org/documentation](https://www.haskell.org/documentation/) page lists
many tutorials, books, guides and courses. You can find a few alternatives that I can
recommend [in this list](https://github.com/soupi/haskell-study-plan#about-this-guide).

## Discussions

Do you want to discuss the book? Maybe ask a question?
Try the [discussion board](https://github.com/soupi/learn-haskell-blog-generator/discussions)!
