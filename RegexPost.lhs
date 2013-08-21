% Regular Expressions from the Ground Up
% Eric Rochester
%

<!--
TODO: add polyfill for <details> http://denis-sokolov.github.io/details-tag/
http://mathiasbynens.be/notes/html5-details-jquery. In the meantime, I'll style
it so I can see it better.
-->

<style>
details { background: hsl(240, 0%, 90%); padding: 10px; display: block; color: hsl(240, 0%, 80%); }
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

A couple of warnings: this isn't a rigorous description of regular expressions
in any sense. It's not theoretically rigorous; it's not practically rigorous
for performance or any other metric. This also isn't a great way to implement
these in Haskell. That would be another interesting exercise (and if there's a
branch on Github for this repository called *haskelly* or something, you know
I've gone down that rabbit hole), but not the one I have planned for today.
Instead, this is supposed to be low-level enough to teach the underlying
concepts of regular expressions, but with a little added sugar to make it
easier to understand.

Literate Programming
--------------------

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

> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE RecordWildCards   #-}
> {-# LANGUAGE NamedFieldPuns    #-}
> {-# LANGUAGE TupleSections     #-}
> {-# LANGUAGE TemplateHaskell   #-}
>
> module RegexPost where
>
> import           Control.Applicative
> import           Control.Lens hiding (re)
> import           Control.Monad.State.Strict
> import qualified Data.List           as L
> import qualified Data.HashMap.Strict as M
> import qualified Data.Text           as T
>
> data Hole = Hole

<div></div></details>

The Data Model
--------------

The first thing we need to do is create the data types for representing the
regular expression abstractly. Everything in regular expressions boils down to
several primitives.

1. Match a literal character (*a*);
1. Concatenation (*a* followed by *b*, usually written *ab*);
1. Alternation (*a* or *b*, written *a|b*);
1. A Kleene star (zero or more *a*, written *a&#x2a;*);
1. An optional element, often denoted by a question mark suffix (*a?*).

> data RegEx
>     = ReLiteral Char
>     | ReConcat RegEx RegEx
>     | ReAlt RegEx RegEx
>     | ReStar RegEx
>     | ReOpt RegEx
>     deriving (Show, Eq)

We can make several of these read a little more naturally, more like the
language we usually use for regular expressions.

> re :: Char -> RegEx
> re = ReLiteral
>
> (.+.) :: RegEx -> RegEx -> RegEx
> (.+.) = ReConcat
>
> (.|.) :: RegEx -> RegEx -> RegEx
> (.|.) = ReAlt
>
> infixr 3 .+.
> infixr 2 .|.
>
> star :: RegEx -> RegEx
> star = ReStar
>
> opt :: RegEx -> RegEx
> opt = ReOpt

This may seem insufficient for creating full-fledged regular expressions, but
we can combine these to build up a richer vocabulary.

Composing Regular Expressions
-----------------------------

In fact, let's see how to compose some of these primitives now.

We've already seen that the Kleene star specifies zero or more of the previous
element. There are other quantifiers for other numbers.

*One or more* elements: This is often represented with a plus sign (*+*). This
is the concatenation of the input regex and it with a star.

> more1 :: RegEx -> RegEx
> more1 rgx = rgx .+. star rgx

*Character classes* are groups of characters, any one of which could match.
This creates a tree of alternatives. It tries each of the characters in the
list and, if none of them matches, failes.

> charClass :: [Char] -> RegEx
> charClass chars = L.foldr1 ReAlt (L.map re chars)

Based on the last definition, we can create some pre-defined character classes:

> whitespace :: RegEx
> whitespace = charClass " \t\n\r"
>
> lowercase :: RegEx
> lowercase = charClass ['a'..'z']
>
> uppercase :: RegEx
> uppercase = charClass ['A'..'Z']
>
> alpha :: RegEx
> alpha = charClass (['a'..'z'] ++ ['A'..'Z'])
>
> digit :: RegEx
> digit = charClass ['0'..'9']
>
> alphaNumeric :: RegEx
> alphaNumeric = charClass (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'])
>
> word :: RegEx
> word = re '_' .|. alphaNumeric

We can combine these to create more complex regular expressions. For example,
here's the regular expression represented by the state machine above, `ab|cd*`.

> eg0 :: RegEx
> eg0 = re 'a' .+. re 'b' .|. re 'c' .+. star (re 'd')

Here's a more complicated example. It would be the regular expression
represented by this PERL-style regex, `\d+\.\d{2}`, which looks for a floating
point number with exactly two positions after the decimal place.

> eg1 :: RegEx
> eg1 = more1 digit .+. re '.' .+. digit .+. digit

When we combine these structures, we're building up the definition of a [state
machine](http://en.wikipedia.org/wiki/State_machine). A state machine is just a
network of nodes. The computer keeps a bookmark pointing to one node. When it
tries to match the input, it takes one character as input, and based on that,
it moves to another node. If a node is marked as a valid stop position, that's
cool: the input matched. If there's no transition for the current input, then
the match fails.

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
<path d="M20,0 a25,20 0 1 1 0,20" stroke-width="2" stroke="hsl(240, 100%, 20%)" fill="none" marker-end="url(#triangle)" />
<text x="73" y="15">d</text>
</g>
</g>
</svg>

We start at the green circle and finish at a red one. At each stage (node), we
take one character of input and move through any out-bound line (edge) that is
labeled with that character. If we end up in a red circle and we've consumed as
much inputas we can, we can stop. If there's no out-bound lines labelled with
the current character and we're not in a red circle, then the regular
expression fails on that input.

We'll implement this state machine in a way that closely follows the diagram.

> type NodeId       = Int
> type NodeEdges    = [RegExEdge]
> type NodeIndex    = M.HashMap NodeId RegExNode

Each `RegExNode` knows whether it's a starting or ending node, and each has a
mapping from characters to more nodes. The entire pattern, and the graph, is
stored in a mapping from node ID to node.

> data RegExNode    = ReNode
>                   { _nodeId    :: NodeId
>                   , _isStart   :: Bool
>                   , _isStop    :: Bool
>                   , _nodeEdges :: NodeEdges
>                   }

Edges come in three flavors.

First, a *character edge* is only traversed when the edge is an outbound edge
from the current node and its `edgeChar` is seen on the input.

> data RegExEdge    = ReCharEdge { _edgeChar   :: Char
>                                , _edgeNodeId :: NodeId
>                                }

Second, *star edges* are followed no matter what input is next.

>                   | ReStarEdge { _edgeNodeId :: NodeId
>                                }

Finally, *null edges* are more like jumps. They don't consume any input, but
whenever they're encountered they automatically shift the current node.

>                   | ReNullEdge { _edgeNodeId :: NodeId
>                                }

The `RegExPattern` then contains the global view of the pattern. It knows which
is the start node, and it maintains an index of the all the nodes. This will be
used when we start traversing the state machine.

> data RegExPattern = RePattern
>                   { _startNodeId :: NodeId
>                   , _nodeIndex   :: NodeIndex
>                   }

The regular expression stored in `RegEx` data structures will be compiled into
a `RegExPattern` state machine. These are the structures that will actually be
used to match input strings against the regular expression.

Creating the State Machine
--------------------------

Compiling the regular expression just involves taking the `RegEx` data
representing the regular expression and generating the state machine graph,
stored in `RegExPattern`. There are several different algorithms for matching
regular expressions and types of state machines to do this.

For this demonstration, we'll use what's called a [*Nondeterministic Finite
Automaton*
(NFA)](http://en.wikipedia.org/wiki/Nondeterministic_finite_automaton). This
just means that we'll take a `RegEx` and construct a graph of `RegExNode`
objects like we saw above. Then, when we're actually matching the input,
instead of walking from one node to another, we walk from one set of nodes to
another. This is useful if the input could match more than one edge into more
than one subsequent node. That might happen for a regular expression like this,
`\w[\w-]*\w` (this matches a word character, a sequence of word characters and
dashes, followed by another word character). The benefit of this is that the
graph can be much smaller and simpler.

The most complicated part of generating the graph is keeping track of an
integer to use for new IDs. Rather than try to thread that data through the
compilation process, we'll use a `State` monad. Don't worry about the scarey
name: it's just a way to pretend like we have a global variable while executing
some functions.

We'll define a type alias to hide the monad even more. Shh. We shouldn't need
to mention it again.

> data CompilerState = CState
>                    { _rePattern  :: RegExPattern
>                    , _nextNodeId :: NodeId
>                    }
>
> type RegExCompiler = State CompilerState

<details><summary>Making Lenses</summary>

Lenses are just a way to interact with Haskell's immutable data structures in a
way that looks a lot like working with most other languages' mutable data
structures. For example, once an instance of `CompilerState` has been created,
its value for `nextNodeId` cannot be changed. Instead we create a new
`CompilerState` with a new value for that slot. Haskell's syntax for that is
awkward (`state { _nextNodeId = 42 }`), but with the `Control.Lens` library, we
can write it more "naturally" (`state & nextNodeId .~ 42`). Of course, in
typical Haskell style, once you go down that rabbit hole, there's a lot more
power. Don't worry about all the strange punctuation you see. Just try to get
the gist of what the lines are doing.

Anyway, these lines actually create the lenses for these data structures.

> makeLenses ''RegExNode
> makeLenses ''RegExEdge
> makeLenses ''RegExPattern
> makeLenses ''CompilerState

<div></div></details>

The `compile` function itself is very simple. It just sets up the environment
and passes execution to the the `compileRegEx` function. Finally, it has to set
the stop flag on the final output nodes.

> compile :: RegEx -> RegExPattern
> compile rgx =
>     (execState (compileRegEx startNode rgx >>= mapM_ setStop) startState) ^. rePattern
>     where startId    = 0
>           startNode  = ReNode startId True False []
>           pattern    = RePattern startId (M.singleton startId startNode)
>           startState = CState pattern (startId + 1)

Before we look at the `compileRegEx` function. let's define a utility. To make
it easier to get a new ID and automatically increment the old one, we'll write
a function to handle that. The function `newNode` builds upon `getNextId` to
return a new node with default values for everything.

> getNextId :: RegExCompiler Int
> getNextId = nextNodeId <<%= (\nId -> nId + 1)
>
> newNode :: RegExCompiler RegExNode
> newNode = do
>     nextId <- getNextId
>     return (ReNode nextId False False [])
>
> refreshNode :: RegExNode -> RegExCompiler RegExNode
> refreshNode node = do
>      idx <- use (rePattern . nodeIndex)
>      return (idx M.! (node ^. nodeId))
>
> insertNode :: RegExNode -> RegExCompiler ()
> insertNode node = rePattern . nodeIndex %= \idx ->
>     M.insert (node ^. nodeId) node idx
>
> addEdge :: RegExEdge -> RegExNode -> RegExCompiler RegExNode
> addEdge edge fromNode = do
>     insertNode newFrom
>     return newFrom
>     where newFrom = fromNode & nodeEdges %~ (\edges -> edge : edges)
>
> addCharEdge :: Char -> RegExNode -> RegExNode -> RegExCompiler RegExNode
> addCharEdge c toNode fromNode =
>     addEdge (ReCharEdge c (toNode ^. nodeId)) fromNode
>
> addStarEdge :: RegExNode -> RegExNode -> RegExCompiler RegExNode
> addStarEdge toNode fromNode =
>     addEdge (ReStarEdge (toNode ^. nodeId)) fromNode
>
> addNullEdge :: RegExNode -> RegExNode -> RegExCompiler RegExNode
> addNullEdge toNode fromNode =
>     addEdge (ReNullEdge (toNode ^. nodeId)) fromNode
>
> setStop :: RegExNode -> RegExCompiler RegExNode
> setStop node = do
>     insertNode stopNode
>     return stopNode
>     where stopNode = node & isStop .~ True

Now, the `compileRegEx` function takes each type of value that a `RegEx` can be
and builds a pattern for it. It returns the same node, but linked to rest of
the graph with the outbound edges. This is necessary because Haskell only uses
immutable data structures, so we can't directly change the input parent node.
Instead we update it, creating a new instance of it, and return that so the
caller has access to it. It also returns all of the final nodes for this.

> compileRegEx :: RegExNode -> RegEx -> RegExCompiler [RegExNode]

For each constructor for `RegEx`, we just need to define the graph created by
each one.

First, `ReLiteral` creates a new node, links to it from the parent node, and
returns the updated pattern.

> compileRegEx parent (ReLiteral c) = do
>     node <- newNode
>     _ <- addCharEdge c node parent
>     return [node]

Second, `ReConcat` creates the first processes the first item, and then it
creates a new set of graphs for the second graph for each child of the first.
That probably makes more sense in code than it does explained (the comments
below may help too).

> compileRegEx parent (ReConcat a b) = do

First compile a and get its children.

>     aChildren <- compileRegEx parent a

Now, compile b, concatenating it onto every child from compiling a. The result
is the updated parent and the updated children from a with the children from
compiling `b` repeatedly.

>     concat <$> mapM (flip compileRegEx b) aChildren

Third, `ReAlt` creates the subgraphs for each of its branches and then
returns all of their children as its children.

> compileRegEx parent@ReNode{..} (ReAlt a b) = do
>     aChildren <- compileRegEx parent a
>     parenta   <- refreshNode parent
>     bChildren <- compileRegEx parenta  b
>     return (aChildren ++ bChildren)

Fourth, `ReStar` creates a loop from a node back to itself.

> compileRegEx parent (ReStar a) = do
>     aChildren <- compileRegEx parent a
>     mapM (addNullEdge parent) aChildren

Finally, `ReOpt`

> compileRegEx parent (ReOpt a) =  do
>     mid <- compileRegEx parent a
>     mapM_ (addStarEdge parent) mid
>     mapM refreshNode mid

Matching
--------

> match :: RegExNode -> T.Text -> Bool
> match = undefined

The Parser
----------

The Sandbox
-----------

> main :: IO ()
> main = putStrLn "howdy!"

