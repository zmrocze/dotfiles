As many in the past I want to re-export module that is imported as qualified. So that by importing my module one gets qualified imports.
For the same question [here](https://stackoverflow.com/q/22973256/10497132) and [here](https://stackoverflow.com/q/3207647/10497132) the answers were that this is impossible.

Did anything change? Is there maybe a language extension?

My case in more detail:

I want to use haskell in shell like I would use awk. I define prelude containing everything I may want to use. A lot of the things clash. I want to write shell oneliners so having to import stuff from within the shell would kill this for me.


There is a solution, just one I don't like. For interpreting haskell I use [hint](https://hackage.haskell.org/package/hint) package. With hint you can specify imports - also qualified - which should be present in the context of the interpreter. So I could specify, everything or parts, as these imports for hint. For some convenience I would want to write these imports in a separate text file and parse it from code instead of directly in code. This is still less ergonomic as imports would not be defined in a haskell source code. In the end I would end up with 2 places for definitions - some prelude source to define custom functions and the import list for hint (somewhere, in code or in file).

In the end I'm re-asking old question to find out if there's some new solutions. Solution can be as hacky as needed as long as the ergonomy in the end is good. Thanks in advance!  
