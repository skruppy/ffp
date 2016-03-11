Sysprak client
==============

[![Build Status](https://travis-ci.org/Skrupellos/ffp.svg?branch=master)](https://travis-ci.org/Skrupellos/ffp)

## Build
1. Get the source
2. Install GTK2 and GTK3 headers and libs.
3. Run `cabal build`

On Debian/Ubuntu you have to run
``` bash
git clone https://github.com/Skrupellos/ffp.git
sudo agt-get install libgtk-3-dev libgtk2.0-dev
cd ffp
cabal configure
cabal build
```

Additionally you can run the tests form the project's root directory by eiter running `cabal test` or, for an fancy output, `runhaskell Sm/Test.hs`.
