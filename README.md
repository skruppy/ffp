Funthello
=========

> Fun‧thel‧lo: A portmanteau word based the abriviation of the lecture "Funktionale Programmierung" and the implemented game "Othello"

This haskell project is the final project of the [Fortgeschrittene Funktionale Programmierung](https://www.tcs.ifi.lmu.de/lehre/ws-2015-16/fun) lecture in 2015/16.
It is based on the project assignment of the [Systempraktikum](http://www.nm.ifi.lmu.de/teaching/Praktika/2015ws/sysprak/) lecture of the same term.

[![Build Status](https://travis-ci.org/Skrupellos/ffp.svg?branch=master)](https://travis-ci.org/Skrupellos/ffp)

## Build
1. Get the source
2. Install gtk2hs-buildtools (requies a recent version of alex and happy).
3. Run `cabal build`

``` bash
git clone https://github.com/Skrupellos/ffp.git
cd ffp
cabal configure
cabal install gtk2hs-buildtools
cabal build
```

Additionally you can run the tests form the project's root directory by either running `cabal test` or `cabal test --show-details=always --test-option=--color` for a fancy colorfull and detailed test results.

## Run
You can run the console appication by calling `cabal run` or executing the binary directly `dist/build/funthello/funthello`, after it has been build.

To run the GUI version of the application, it has to be called with an appication name ending in "-gui".
This can be done by running `funthello-gui` after a `cabal build`.
