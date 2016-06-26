---
kind:         article
published:    2016-06-26
title:        Working with GHC Core
author:       Alberto Sadde
tags:         programming, Haskell, GHC, compilers
metaKeywords: test
theme:        default_syntax
---

**Introduction**

While very concise and powerful, GHC's intermediate language Core is very rich
and hard to tame, at times. This post is about my quest to manipulating Core and
implementing an abstract machine to interpret the language.

I will first describe Core in its latest incarnation and motivate the need for understanding and using it. Then I will discuss my endevaours using it and
go over some of the difficulties I encountered while using it.

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


<!-- Why Core is interesting?
  Compiler Plugins?
-->


