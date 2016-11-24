# UI effects

An experiment exploring a UI programming model inspired by algebraic effects.


## Languages

An algebraic effect system such as [Oleg Kiselyov’s presentation](http://okmij.org/ftp/Haskell/extensible/) might model individual effects as the combination of a functor and a handler function which performs the actions represented in the datatype. In like fashion, the current work represents each aspect of UI programming—layouts, drawings, interactions, etc.—as a functor with an associated function to perform its actions.

Where we start to diverge is to think of these functors as being _languages_ instead of _effects_, and of their corresponding functions as _interpreters_ instead of _handlers_.

To be precise, each language is an embedded DSL. Each functor has associated “smart” constructors which wrap a single, nonrecursive value up into a recursive structure, and the result is an idiomatic Haskell API presenting the facilities offered by the language in question.
