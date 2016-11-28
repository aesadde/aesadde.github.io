---
title: Haskell-Stats
published: 2016
github: https://github.com/aesadde/haskell-stats
---
This is a project that spawned from a collections of scripts that I used to
gather data for my masters thesis. It is an ongoing project and its far from
complete.

<div class="read-more"> <a href="#" id="showDiv">Read more</a> </div>

<div id="proj-details">
<span class="proj-info">Objectives:</span>
<ul>
<li> To provide a query engine (in the style of Hoogle) that gives information about
all Hackage packages.
<li> The engine will provide ways to gather information such as most used and downloaded packages and modules; most called functions;
</ul>

<span class="proj-info">Current Status:</span>
<ul>
<li> Architecture change: the current version of the engine assumes a downloaded list of packages. We are working to use directly the hackage database.
<li> API: we are building an extensible API so that users can implement their own queries on the data.
</ul>
</div>
