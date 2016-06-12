---
kind:           article
published:      2016-06-11
title: Making sense of the GHC API
author: Alberto Sadde
tags: programming, Haskell, GHC, compilers
metaKeywords: test
theme: default_syntax
---
**Introduction**

Being already at the 8.01 release of the GHC compiler, one would expect
to find a lot of information and examples showing how to use the GHC
API. But this doesn't seem to be the present case. While there exist
some entries in the [Haskell
Wiki](https://wiki.haskell.org/GHC/As_a_library) and a somewhat outdated
[commentary](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/API),
there doesn't seem to be much more explained out there.

I've read other blog posts that try to explain bits and pieces (look for
links at the bottom of the above Haskell Wiki entry) but none of them
tackle the problem I am trying to solve.


**Extracting Core from Packages**

For my masters thesis I am trying to build a query Engine for Haskell
functions/modules/packages.
In particular I am interested in knowing which functions are called the
most inside a package and inside a module in order to get some useful
statistics. But I am not interested in extracting this information at
the source code level, rather, I am interested in extracting this
information at the Core level.


[GHC Core](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType) is
Haskell's intermediate language. (I hope to write an in-depth post on
Core as well). For our current discussion, it suffices to say that GHC
Core is a small, explicitly typed functional language. This is the
language that all our source code gets *desugared* to before applying
optimizations, which in GHC amount to a set of Core -&gt; Core
transformations.


So, what I really want is not merely compiling a Haskell package (we can
use `stack` or `cabal` for this) but I want to compile and get the
Core of each function defined in each module within the package and add
all this information to a database on which we can make then queries and
build our stats.


**The GHC API**

I will be using GHC 7.10.3 (since all the code was tested with it. I'll
update this entry if there are significant changes once I move to GHC
8). All the documentation is available
[here](http://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/index.html).


As noted in the GHC commentary (linked above), the compilation process
consists of the following steps:

1.  Create a new session
2.  Set and/or get the session's flags.
3.  add and load the compilation targets
4.  Perform dependency analysis to understand what needs to be compiled first
5.  load the targets.

We will now go through all of these steps in turn since they involve
quite a lot of different functions and a lot of tweaking to get
everything to compile. We have successfully managed to compile files
inside packages but we have been unable to compile non-exposed modules
of a package (more on this later).


**Session**

Since the compilation process is rather stateful, everything happens
inside the [GhcMonad](http://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/GHC.html#t:GhcMonad).
Here we need to create a session and set up the environment. 


Luckily, session creation is handled by the `runGhc` function so we
don't really need to do anything here. This function calls
[`initGhcMonad`](http://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/GHC.html#v:initGhcMonad) which
does all the job for us with all the standard environment set up for us.


**Flags**

Setting up the flags is probably the single most important step in
setting up all the environment to compile the files. The `DynFlags`
data structure is extensive and, depending on your needs, you might want
to inspect it more closely.


The code to initialize the GhcMonad session and set up the flags is
given below:

``` haskell
runEngingeGhc :: Package -> GHC.Ghc a -> IO a
runEngingeGhc pkg act =
  GHC.defaultErrorHandler defaultFatalMessager defaultFlushOut $
    withSystemTempDirectory "engine" $ \tmp ->
    GHC.runGhc (Just libdir) $ do
      df <- GHC.getSessionDynFlags
      let df' = df { importPaths  = nub $ includeDirs pkg ++ importPaths df
                   , libraryPaths = nub $ includeDirs pkg ++ libraryPaths df
                   , includePaths = nub $ includeDirs pkg ++ includePaths df
                   , packageFlags = [ ExposePackage (PackageArg "ghc-prim")
                                                    (ModRenaming True [])
                                    , ExposePackage (PackageArg "ghc-paths")
                                                    (ModRenaming True [])]
                                    ++ packageFlags df
                   , ghcLink      = LinkInMemory
                   , ghcMode      = CompManager
                   , objectDir    = Just tmp
                   , hiDir        = Just tmp
                   , stubDir      = Just tmp
                     }
      _ <- GHC.setSessionDynFlags df'
      act
```


Since we are compiling more than one module we need to set the
`gchMode` flag to `CompManager` which is equivalent to `--make`
when compiling from the command line.
We set `ghcLink` to LinkInMemory which amounts to doing dynamic
linking of the libraries. Also important are the exposed packages defined in
`packageFlags`, these expose a few packages needed by our program that otherwise
would be hidden (we can achieve a similar result using `ghc-pkg expose
package`).


The rest of the set up is straightforward, we add paths to the includes
and libraries directories so the compiler knows where to look and set a
temporary directory for the compiler to work in.


**Targets**

The only thing left is to define the "action" to pass to the above
function. This action is wrapped inside the GhcMonad and runs with all
the settings defined above.

```haskell
getGhcInfo :: FilePath -> GHC.Ghc ModGuts
getGhcInfo target = do
  GHC.setTargets . return =<< GHC.guessTarget target Nothing
  _ <- GHC.depanal [] False
  GHC.load GHC.LoadAllTargets
  makeModGuts target
```

Given a path to a module, the above function first creates a `Target`
from the String (FilePath in our case) given, we then set the targets
for compilation.


**Dependency Analysis**

After setting up the targets, we need to decide which targets to compile
first, as there might be some dependencies between the modules. This is
done by `depanal` [which traverses the transitive dependencies of the
module (by reading the import directives in the
file).](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/API#DependencyAnalysis) 


**Load and compile**

Finally we simply load the target and compile 

```haskell
makeModGuts :: FilePath -> GHC.Ghc ModGuts
makeModGuts f = do
  modGraph <- GHC.getModuleGraph
  case find (\m -> not (isBootSummary m) && f == msHsFilePath m) modGraph of
    Just modSummary -> do
      parsed   <- GHC.parseModule modSummary
      d <- GHC.desugarModule =<< GHC.typecheckModule parsed
      return $! GHC.coreModule d
    Nothing ->
      panic "Ghc Interface: Unable to get GhcModGuts"
```

The compilation generates a module graph, this is stored as a list of
[ModSummary](http://downloads.haskell.org/~ghc/7.10.3/docs/html/libraries/ghc-7.10.3/GHC.html#t:ModSummary) from
which we extract the one we need and last but not least we parse,
typecheck and desugar the module into Core.


**Final Remarks **

The above code was largely [taken from Liquid
Haskell](https://github.com/ucsd-progsys/liquidhaskell/blob/master/src/Language/Haskell/Liquid/GHC/Interface.hs) and
I spent quite a few days trying to make sense and understand the inner
workings of the compiler and the GhcMonad.


With the above code I am able to compile entire packages with some big
exceptions:

-   Modules inside packages that depend on non-exposed modules fail
    to compile. At first I thought this was something to do with the
    include/library directories, but that turned out not to be
    the problem. As of this writing I have yet to find a solution to
    this problem.\
    \
-   Modules that contain .hsc files fail to compile. For now I have
    found  a manual solution: compile each .hsc file using the command
    line tool `hsc2hs`. 


If you know how to improve the above code or how to solve the problems I
listed here, please do get in touch!
