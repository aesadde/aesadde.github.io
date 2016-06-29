---
kind:         article
published:    2016-06-29
title:        Working with GHC Core
author:       Alberto Sadde
tags:         programming, Haskell, GHC, compilers
metaKeywords: test
theme:        default_syntax
---

**Introduction**

While very concise and powerful, GHC's intermediate language, Core is very rich
and hard to tame, at times. This post is about my quest quest to understand and
manipulate Core while implementing an abstract machine to interpret the language
(more on this maybe later).

I will first describe Core in its latest incarnation and motivate the need for
understanding and using it.  Then I will discuss my endevaours using it and go
over some of the difficulties I encountered while using it.

**What is GHC Core**

Like many modern compilers the GHC Haskell compiler is built as a pipeline of
consecutive program transformations. [While we won't go over the compiler in
details](http://www.sciencedirect.com/science/article/pii/S0167642397000294),
GHC's pipeline parses, typechecks, desugar and finally simplifies the code.
When desugaring the code, GHC converts all plain Haskell source code into a much
simpler language, GHC Core.

Core is an explicitly typed functional language based on [System Fc](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/tldi22-sulzmann-with-appendix.pdf),
a variant of System Fw with equality constraints and coercions. In GHC, Core is
indeed a very simple language:

```haskell
type CoreExpr = Expr Var
data Expr b	-- "b" for the type of binders,
  = Var	  Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b Type [Alt b]
  | Cast  (Expr b) Coercion
  | Tick  (Tickish Id) (Expr b)
  | Type  Type

type Arg b = Expr b
type Alt b = (AltCon, [b], Expr b)

data AltCon = DataAlt DataCon | LitAlt  Literal | DEFAULT

data Bind b = NonRec b (Expr b) | Rec [(b, (Expr b))]
```

Then, together with a simple type for binders
```haskell
data Bind b = NonRec b (Expr b)
            | Rec [(b, (Expr b))]
```
a Haskell program gets desugared into a `CoreProgram` which is simply a synonym
for `[CoreBind]`, which in turn is a synonym for `Bind CoreBndr` where `type CoreBndr = Var`.

**GHC Plugins**

The importance of this language lies in its simplicity: since it is
much simpler than pure Haskell, applying transformations to a Core
program should be a much simpler task. So simple, in fact, that GHC provides an
[API](http://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/GhcPlugins.html)
so that developers can write their own [compiler plugins](https://downloads.haskell.org/~ghc/7.10.3/docs/html/users_guide/compiler-plugins.html).
This is a great deal given that we can simply insert our plugin at any stage
during the optimization pipeline to apply the transformation we want. There are
several good examples such as [the Herbie plugin](https://github.com/mikeizbicki/HerbiePlugin#herbie-ghc-plugin) that "improves the numerical stability of Haskell programs", or
[this](https://github.com/thoughtpolice/strict-ghc-plugin) plugin which enables
to make functions  (previously annotated) strict.

** Understanding Core **

Despite its simplicity, taming Core can be hard.

First of all, while there is an API, the documentation is very poor and there is
a generalized lack of examples. This is understandable (up to a point) given
that writing compiler plugins is not a common task and something that not even
the most experienced Haskell programmers would do on a normal day.

The most up to date description of the language, as far as I am aware is ["An
external representation of GHC Core"](https://downloads.haskell.org/~ghc/6.12.2/docs/core.pdf), a description of Core as used in GHC 6.10.
Considering that we are already in GHC 8, this is not a promising outlook given
that outdated documentation makes it even harder to read and understand a Core
program such as produced by the flag `ddump-ds`.

Take for example the following simple program:
```haskell
-- Example.hs
module Example where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)
```

Compiling with `ghc --make -ddump-ds Example.hs` gives us the following:
```haskell
==================== Desugar (after optimization) ====================
Result size of Desugar (after optimization)
  = {terms: 20, types: 8, coercions: 0}

Rec {
factorial [Occ=LoopBreaker] :: Integer -> Integer
[LclIdX, Str=DmdType]
factorial =
  \ (ds_dKE :: Integer) ->
    case ==
           @ Integer
           integer-gmp-1.0.0.0:GHC.Integer.Type.$fEqInteger
           ds_dKE
           (__integer 0)
    of _ [Occ=Dead] {
      False ->
        (\ _ [Occ=Dead, OS=OneShot] ->
           * @ Integer
             GHC.Num.$fNumInteger
             ds_dKE
             (factorial
                (- @ Integer GHC.Num.$fNumInteger ds_dKE (__integer 1))))
          GHC.Prim.void#;
      True -> __integer 1
    }
end Rec }
```

This code dump contains a lot of information about the Haskell source code but, the
untrained developer will have a hard time to deciphering all the bits and pieces
of this text. Since my attempt here is not to teach you the "basics" of Core but
rather, understanding how to manipulate it, you can check [Stephen Diehl's
introduction to Core](http://dev.stephendiehl.com/hask/#core) to get you
started.

