
# Regular Expressions from the Ground Up

This is a blog post I'm doing as a literate programming exercise. The literate
Haskell file can generate several things:

1. [Pandoc](http://johnmacfarlane.net/pandoc/) creates the HTML5 blog post.
2. [GHC](http://www.haskell.org/ghc/) creates a native Haskell library, which
   is used for running the Specs in the `specs` directory; and
3. [GHCJS](https://github.com/ghcjs/ghcjs) creates a JavaScript library that
   uses the regular expression engine defined in the post to animate the form
   at the end of the post for plalying with the subject of the post.

