# Where to go next

Haskell is an incredibly rich and deep programming language.
New cutting-edge techniques, concepts and features are still being discovered
and sometimes integrated into GHC. This sometimes makes it seemingly impossible
to catch up to.

This phenomena is sometimes dubbed
[The Haskell pyramid](https://patrickmn.com/software/the-haskell-pyramid/).
My hope is that by reading this book and following the exercises,
you readers have reached the bar of productivity, and you can now go and start
working on your own projects with Haskell. I highly encourage you to do so.
In my opinion, writing useful Haskell projects is the best method to solidify
what you currently know, and identify what you still need to learn.

## Extending this project

If you'd like to extend this project, here are a few ideas for you:

1. **Serve over HTTP** - You can use a web library such as
   [wai](https://www.youtube.com/watch?v=mz5_HmLGRXc) or
   [twain](https://gilmi.me/blog/post/2022/04/24/learn-twain-bulletin-app)
   to serve this blog over HTTP instead of generating it statically
2. **Rewrite it with libraries** - you could rewrite it and use a real-world
   [HTML package](https://hackage.haskell.org/package/lucid)
   and [markdown parser](https://hackage.haskell.org/package/cmark-gfm)
3. **Add features**
   1. You could add a metadata block at the top of each article
      which would include the title, publish date and tags of a blog post,
	  and use them when generating HTML, index page and even tags pages
   2. You could add HTML pages templating using
      [mustache](https://hackage.haskell.org/package/mustache) or similar,
	  and use that to generate a nice and customizable structure to the page
   3. You could add support for links and images in our markup language parser
   4. You could add support for configuration files which would include things like
      the blog title, description, or other meta information for things like
	  [twitter cards](https://developer.twitter.com/en/docs/twitter-for-websites/cards/overview/abouts-cards)

Or anything else you can think of, consider this project your playground and
do whatever you like with it!

## Other resources

At some point you are likely to run into new concepts, techniques,
or even just a feeling of "I feel like this could be done better".
I'd like to point you in the right direction so you can find additional information
and learn new Haskell things when you need to or want to.

I've compiled a list of resources for learning Haskell called
[Haskell study plan](https://github.com/soupi/haskell-study-plan),
which includes links to very useful articles, community hubs and news aggregators,
project suggestions, and even cool open-source Haskell projects.
You will also find alternative explanations to thing we've covered
and even links to other Haskell tutorials, guides and books in case you need
a different view on things.

Also, the [GHC User Guide](https://downloads.haskell.org/ghc/latest/docs/users_guide/index.html)
is a fantastic resource with loads of articles and information about the language and GHC tooling around it.
It is often the best place to learn about the Haskell language.

However, don't feel pressured to learn everything Haskell
has to offer right away. Mastering Haskell is a journey that can take a lot of time.
Most of us are definitely not there yet, but we can still be very productive with Haskell,
build real-world projects, and even discover new techniques and concepts ourselves.

Remember that in a lazy language we evaluate things only when we need them.
Maybe we can do that too with Haskell concepts!
