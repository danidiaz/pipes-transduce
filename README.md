## What's in this library?

A **Fold'** datatype that consumes
[Producers](http://hackage.haskell.org/package/pipes-4.1.7/docs/Pipes.html#t:Producer)
from [pipes](http://hackage.haskell.org/package/pipes) and can be constructed
in a variety of ways, in particular from the more versatile folds in Gabriel
Gonzalez's [foldl](http://hackage.haskell.org/package/foldl) package.

## When to use this library?

I wanted a fold-like datatype for Producers that let me perform "bracketing"
operations (the folds in foldl are push-based and do not allow that) and had
"stopping on error" behaviour already baked in.

If you don't need any of that, you are better off just using **pipes** and/or
**foldl** by themselves. 

## Where can I find working examples for this library?

There are none yet.
