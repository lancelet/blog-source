---
title: FP vs OO; A Better Way to Count Candy
description: Simple discussion of FP code organization
published-on: 2018-11-24
author: Jonathan Merritt
tags: Python, Haskell, Architecture, Monoid, Catamorphism
---

After weeks of frustration, I've decided it's worthwhile blogging about a
pattern I've noticed in some Python code I'm working with.  I can't provide the
*real* code, but I can describe the problem with an example.  I'm going to
discuss how the Python code works, it's pros-and-cons, and then describe how I'd
approach the same problem in Haskell.  This is all very simple stuff, but it's
been driving me so crazy recently that I feel the need to write about it.

The complete Python and Haskell code (as executable scripts) for this blog post
[is available as a gist](https://gist.github.com/lancelet/0ef0aae251f7831b23da7030ff2dbed3).

## The Task

Our toy problem is going to involve counting jellybeans and jellybabies. We're
going to be given a list of candy bags:

~~~~{.python}
# Python
candy_bags = [
    { 'jellybeans': 42,
      'jellybabies': 10,
      'frogs': 8 },
    { 'jellybeans': 2,
      'jellybabies': 8,
      'nerds': 11 } ]
~~~~

Our job is to produce two reports; one is a `full_report.txt`:

~~~~
JellyBeans:  44
JellyBabies: 18
Beans per Baby: 2.44
~~~~

The other is a `summary.txt`:

~~~~
Beans per Baby: 2.44
~~~~

Why two reports, when one contains all the information? Well, some manager asked
for them, that's why! We are merely Software Engineers; it's not our place to
reason about The Business (TM) in the complicated world of Big Candy (TM)!

## Legacy Python Solution

So how does the legacy Python code work? It looks like this:

~~~~{.python}
# Python

class JellyCounter:

    def __init__(self, report_path: str):
        self.beans_count = 0
        self.babies_count = 0
        self.beans_per_baby = 0.0
        self.report_path = report_path

    def add_bag(self, candy_bag):
        self.beans_count += candy_bag['jellybeans']
        self.babies_count += candy_bag['jellybabies']

    def calculate_fraction(self):
        self.beans_per_baby = self.beans_count / self.babies_count

    def write_summary(self):
        self.calculate_fraction()
        with open(f'{self.report_path}/summary.python.txt', 'w') as out_file:
            out_file.write(f'Beans per Baby: {self.beans_per_baby:.2f}\n')

    def write_full_report(self):
        with open(f'{self.report_path}/full_report.python.txt', 'w') as out_file:
            out_file.write(f'JellyBeans:  {self.beans_count}\n')
            out_file.write(f'JellyBabies: {self.babies_count}\n')
            out_file.write(f'Beans per Baby: {self.beans_per_baby:.2f}\n')


if __name__ == '__main__':
    counter = JellyCounter(report_path='.')
    for bag in candy_bags:
        counter.add_bag(bag)
    counter.write_summary()
    counter.write_full_report()
~~~~

Although I can imagine the groans already (and ignoring all the exceptions it
might throw)... this isn't *all* bad. Let's look at the pros and cons:

**Pros**

1. The code is all contained in the one class. This isn't "modularity" as I
   think of it, but it's what a lot of OO people mean, and it's worth
   *something*... at least I only have to look in one place.

2. There are very few restrictions on how we use the `JellyCounter`. We can hold
   it as state and use it to track web requests, we can use it over a list (as
   in the actual use-case), etc. It's pretty versatile.
   
3. We can be confident about runtime behaviour (eg. memory use, etc).

**Cons**

1. The code has a "lifecycle". By this, I mean that we have to call the methods
   in the correct order. If, for example, we were to call `write_full_report`
   before `write_summary`, we'd be in trouble (one of many ways it could fail).
   We could defend against that mistake and others in various ways, but all
   would add extra overhead to an otherwise simple structure.

2. The code does not separate concerns very well. Counting our candy and
   reporting on those counts are separate things (even in *this* code, they're
   separate), yet we cram them into one class because we follow the OO crede
   that they operate on *some* of the same data and therefore belong in the same
   place.

3. We store state that probably doesn't need to be stored.

4. The code is difficult to test without running it through its lifecycle and
   doing real IO with it.

Now before I go any further, I probably need to recap that this is just a toy
example. The real code I'm talking about is much more complicated than this. The
code base contains probably around 100 classes like this, some of which are
nested in interesting ways, and all of which do more operations than just
counting. But the basic structure and the "Pros" and "Cons" listed above are the
same.

You could, of course, structure this Python code a little better. For example,
structuring the computation of `beans_per_baby` in a more robust way would
improve it an awful lot. However, I've written it like this because it's *very*
easy (practically *guaranteed*, IMO), for badly-structured code like this to creep
in when squads are under delivery pressure for one-point tickets.

## The Haskell Way

So how would I structure the code? Well, 10 years ago, I might have written code
just like the Python above, but now I've been exposed to FP for a while, I know
there's A Better Way (TM).

First of all, I recognize that we can structure this whole thing in a slightly
different way. We can do an operation on each of the input values (extracting a
count from each candy bag), and then smash together these counts to produce a
total. Finally, having the total, we can report on it. The first part of that
process is a very classic pattern in FP, that people variously refer to as a
fold, a map-reduce or a catamorphism. (If you're starting out in FP, don't
stress about the terminology. It sounds complicated sometimes, but it's mostly
about patterns. Once you recognize the patterns, you'll want the right names for
them.)

To begin with, some imports and simple language extensions:

~~~~{.haskell}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Double.Conversion.Text (toFixed)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.IO                (writeFile)
import           System.IO                   (FilePath)
import           TextShow                    (showt)

import           Prelude                     hiding (writeFile)
~~~~

Let's start with a Haskell definition of a candy bag to mirror what we had in
Python:

~~~~{.haskell}
newtype CandyBag = CandyBag (Map Text Int)

candyBags :: [CandyBag]
candyBags =
    [ CandyBag (Map.fromList
        [ ("jellybeans", 42)
        , ("jellybabies", 10)
        , ("frogs", 8) ])
    , CandyBag (Map.fromList
        [ ("jellybeans", 2)
        , ("jellybabies", 8)
        , ("nerds", 11) ])
    ]
~~~~

Now let's introduce a type for the counts of jellybeans and jellybabies:

~~~~{.haskell}
data JellyCount
    = JellyCount
    { beans  :: Int
    , babies :: Int
    }
~~~~

With these definitions out of the way, given a single `CandyBag`, we can find
its `JellyCount`:

~~~~{.haskell}
countJelly :: CandyBag -> JellyCount
countJelly (CandyBag m) = JellyCount beans babies
  where
    beans = Map.findWithDefault 0 "jellybeans" m
    babies = Map.findWithDefault 0 "jellybabies" m
~~~~

Now we need a way to smash together two `JellyCount`s to produce another one
which contains their combined count. This behaviour is captured by a pair of
type classes in Haskell called `Semigroup` and `Monoid`, which together capture
another functional pattern which is extremely common:

~~~~{.haskell}
instance Semigroup JellyCount where
    -- how to we smash two together?
    JellyCount a b <> JellyCount c d = JellyCount (a + c) (b + d)

instance Monoid JellyCount where
    -- how do we get an empty JellyCount?
    mempty = JellyCount 0 0
~~~~

The Monoid provides an extra notion: the idea of an "empty count". In this case,
the empty count makes a lot of sense, so I'm defining it as well.

Monoids are not *only* about smashing things together. They should also obey
[some simple
laws](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html).
The reason for this is that, because our monoid obeys the laws, we get a lot of
behaviour for free from built-in Haskell functions. This could go wrong if we
didn't obey the laws, so we should be careful about them. You'll see some of
this "built-in" stuff below with the `mconcat` function. In short, a "lawful"
type class is like the FP version of a "really, really well-defined interface"
in OO. Suffice to say, this definition does obey the semigroup and monoid laws.

Finally, if we have a `JellyCount` we might wish to process it a bit before we
serialize it. The processing in this case corresponds to computing the "beans
per baby" metric that is so critical to our Business Outcome (TM) and Delivering
Value For The Client (TM). Let's introduce a `SummarizedJellyCount` to mirror
what we did in Python:

~~~~{.haskell}
data SummarizedJellyCount
    = SummarizedJellyCount
    { basicCount   :: JellyCount  -- our basic information
    , beansPerBaby :: Double      -- extra information (ooo... expensive!)
    }

summarize :: JellyCount -> SummarizedJellyCount
summarize j = SummarizedJellyCount j bpb
  where
    bpb = fromIntegral (beans j) / fromIntegral (babies j)
~~~~

Now we can write the final serialization operations:

~~~~{.haskell}
serializeSummary :: SummarizedJellyCount -> Text
serializeSummary (SummarizedJellyCount _ bpb)
    = "Beans per Baby: " <> toFixed 2 bpb <> "\n"

serializeFullReport :: SummarizedJellyCount -> Text
serializeFullReport s@(SummarizedJellyCount (JellyCount beans babies) _)
    = "JellyBeans:  " <> showt beans <> "\n"
   <> "JellyBabies: " <> showt babies <> "\n"
   <> serializeSummary s

writeSummary :: FilePath -> SummarizedJellyCount -> IO ()
writeSummary fp s = writeFile fp (serializeSummary s)

writeFullReport :: FilePath -> SummarizedJellyCount -> IO ()
writeFullReport fp s = writeFile fp (serializeFullReport s)
~~~~

And finally, this is how we use it:

~~~~{.haskell}
main :: IO ()
main = do
    let

        -- We make a list of JellyCount, one for each bag of candy.
        --   fmap is a standard function which applies our countJelly
        --   function to each CandyBag in the list, producing a list as
        --   output.
        counts :: [JellyCount] = fmap countJelly candyBags

        -- We smash together all the counts to get the final count.
        --   mconcat is another standard function that lets us smash
        --   together a whole list that contains monoid elements.
        finalCount :: JellyCount = mconcat counts

        -- Do the summary
        summarizedCount = summarize finalCount

    -- The IO actions which create the reports:
    writeSummary "summary.haskell.txt" summarizedCount
    writeFullReport "full_report.haskell.txt" summarizedCount
~~~~

So what did all the Haskell stuff buy us?

1. There are no longer any lifecycle concerns. We have used the type system to
   ensure that things will work the right way, and that the flow of information
   is correct. For example, we can't call `writeSummary` or `writeFullReport`
   with a `JellyCount`. We can only call those functions with a
   `SummarizedJellyCount`, which we get only after providing the summary data.
   We can lean on the type system in this way to enforce the "lifecycle" /
   "ordering" concerns that were so tenuous in Python.
   
2. Concerns are now separated. The separation is nicely visible in the types:

    - `CandyBag -> JellyCount`; here we produce a count from a bag,

    - `instance Semigroup JellyCount`; this is how we smash two counts together,
    
    - `instance Monoid JellyCount`; ... and we can have a zero count,

    - `JellyCount -> SummarizedJellyCount`; here we describe how to summarize a
      count,

    - `FilePath -> SummarisedJellyCount -> IO ()`; and here we do IO with our
      summarized count somehow.

3. State management is much clearer.

4. The code is much easier to test than the Python version. We can test each
   part in complete isolation, without needing to do any crazy mocking, or
   providing class parameters that won't be used by individual unit tests.

5. There are some minor safety improvements, such as `Map.findWithDefault`. (The
   Python version could be improved too; this is only a minor thing in the
   context of what I'm discussing here.)

## Summary

That's it; there's not much more to say. Most of what I've covered here is
super basic for FP people, but I think it's probably quite obscure for those
with an OO / Python background. Hopefully this post helped somebody out there to
understand the frustrations I've been having with this code, and how it's
possible to structure operations like this a lot more simply.

Please remember that the above is just a toy example; I'm trying to illustrate
the *structure*, not necessarily this *specific* problem.
