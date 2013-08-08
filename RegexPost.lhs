
<!--
TODO: add polyfill for <details> http://denis-sokolov.github.io/details-tag/
http://mathiasbynens.be/notes/html5-details-jquery. In the meantime, I'll style
it so I can see it better.
-->

<style>
details { background: hsl(240, 0%, 90%); padding: 10px; display: block; }
summary { display: block; font-weight: bold; }
</style>

The other day, I helped [Jeremy](http://www.twitter.com/clioweb) work through a
regular expression. At the end of it, he posted this tweet:

![The Out-of-Control Tweet](i/tweet.png)

This caused a discussion about using regexes to work on HTML or XML. I should
mention that I'm against it in principle, but this was a good, narrowly focused
use, we felt. There was also talk about acid and [The
Ring](http://www.imdb.com/title/tt0298130/).

But the more interesting discussion took place while we were building the
regular expression, and we talked about how to piece one together and how to
understand what it does.

(As an aside, evidently, Jeremy's my muse. I only post here after some
discussion we have. We need to talk more, Jeremy.)

In general, for almost any piece of computer technology, understanding it
requires some amount of finding out what happens under the table. Computer
program is a stack of abstractions, but all abstractions are [leaky
abstractions](http://www.joelonsoftware.com/articles/LeakyAbstractions.html).
Knowing something about C helps you program in Python or Ruby, not because C
programmers are so great, but because you understand something about the how
Python and Ruby were created, what choices their developers had available to
them, and why they designed those languages the way they did.

In this case, knowing how a regular expression works helps to construct them.
It helps you see the options at each point and to anticipate what the computer
will do while trying to match against some text. And what better way to
understand a system like this than to build a toy example and play with the
output. That's what we'll do in today's (rather long---sorry---post).

Literate Programming
====================

To make this a real interactive page, this will be a [literate
programming](http://en.wikipedia.org/wiki/Literate_programming) post. The page
and the program it's written in are processed to create the program and the
post. Moreover, since the text will become a web page and the code will become
JavaScript, the output will call and embed the output of the code into the
post. Hence, at the end, you'll be able to play with the code described in this
post directly.

It also means that you can see the history of this post and how it was built by
browsing through the commits for this post's project [on
Github](https://github.com/erochest/regex-post). Feel free to fork it and add
your own features or play with it further.

<details><summary>Haskell Header</summary>

> module Main where
> 
> import qualified Data.Map as M

<div></div></details>

The Data Model
==============

The first thing we need to do is create the data types for representing the
regular expression abstractly. Everything in regular expressions boils down to
several primitives.

1. Match a literal character (*a*);
2. Concatenation (*a* followed by *b*, usually written *ab*);
3. Alternation (*a* or *b*, written *a|b*); and
4. A Kleene star (zero or more *a*, written *a&#x2a;*).

> data RegEx
>     = RELiteral Char
>     | REConcat RegEx RegEx
>     | REAlt RegEx RegEx
>     | REStar RegEx
>     deriving (Show)

These primitives are combined together into a [state
machine](http://en.wikipedia.org/wiki/State_machine). A state machine is just a
network of nodes. The computer keeps a bookmark pointing to one node. When it
tries to match the input, it takes one character as input, and based on that,
it moves to another node. If a node is marked as a valid stop position, that's
cool: the input matched. If there's no transition for the current input, then
the match fails.

> type NodeId     = Int
> type NodeEdges  = M.Map Char NodeId
> type NodeIndex  = M.Map NodeId RegExNode
> data RegExNode  = RENode
>                 { nodeId    :: NodeId
>                 , isStart   :: Bool
>                 , isStop    :: Bool
>                 , nodeEdges :: NodeEdges
>                 } deriving (Show)
> data RegExState = REState
>                 { startNode :: NodeId
>                 , nodeIndex :: NodeIndex
>                 }

In practice, there are more things than the four we listed above, but those are
implementation details for performance. In theory, character classes and other
things are build by combining those four things with transitions in the state
machine. We'll see some examples of that below.

For example, here's one way to represent the regular expression `ab|cd*` as a
state machine. This would match either the string *ab* or *c* followed by any
number of *d*'s, including no *d*'s. So *ab*, *c*, *cd*, or *cddddd* would all
match.

<svg width="300" height="200" viewPort="0 0 300 200">
<defs>
<marker id="triangle" viewBox="0 0 10 10" refX="1" refY="5" markerWidth="4" markerHeight="4" orient="auto">
<path d="M 0 0 L 10 5 L 0 10 z" />
</marker>
</defs>
<circle cx="60" cy="100" r="20" style="fill: hsl(120, 100%, 50%); stroke-width: 3; stroke: hsl(120, 100%, 20%)" />
<g style="fill: white; stroke-width: 3; stroke: hsl(240, 100%, 20%)">
<circle cx="120" cy="60" r="20" />
</g>
<g style="fill: hsl(0, 100%, 50%); stroke-width: 3; stroke: hsl(0, 100%, 20%)">
<circle cx="120" cy="140" r="20" />
<circle cx="180" cy="60" r="20" />
</g>
<g font-family="Verdana" font-size="13">
<g transform="translate(60 100)">
<g transform="rotate(325)">
<line x1="20" y1="0" x2="45" y2="0" stroke-width="2" stroke="hsl(240, 100%, 20%)" marker-end="url(#triangle)" />
<text x="31" y="-5">a</text>
</g>
<g transform="rotate(35)">
<line x1="20" y1="0" x2="45" y2="0" stroke-width="2" stroke="hsl(240, 100%, 20%)" marker-end="url(#triangle)" />
<text x="28" y="-5">c</text>
</g>
</g>
<g transform="translate(120 60)">
<line x1="20" y1="0" x2="33" y2="0" stroke-width="2" stroke="hsl(240, 100%, 20%)" marker-end="url(#triangle)" />
<text x="24" y="-5">b</text>
</g>
<g transform="translate(120 140)">
<path d="M20,0 a15,20 0 1 1 0,20" stroke-width="2" stroke="hsl(240, 100%, 20%)" fill="none" marker-end="url(#triangle)" />
<text x="53" y="15">d</text>
</g>
</g>
</svg>

We start at the green circle and finish at a red one. At each stage (node), we
take one character of input and move through any out-bound line (edge) that is
labeled with that character. If we end up in a red circle and we've consumed as
much inputas we can, we can stop. If there's no out-bound lines labelled with
the current character and we're not in a red circle, then the regular
expression fails on that input.

Creating the State Machine
==========================

Matching
========

The Parser
==========

The Sandbox
===========

> main :: IO ()
> main = putStrLn "howdy!"

