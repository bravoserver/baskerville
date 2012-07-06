Baskerville
===========
A Minecraft server written in haskell.

Features
========

Standard
--------
* User can join and the client gracefully times out

Extended
--------
* Coming soon

Build
=====

Get the Haskell Platform
------------------------
**OSX and Windows**

    http://hackage.haskell.org/platform/

**Linux**

    Use your package manager to install haskell-platform or get it at http://hackage.haskell.org/platform/

Build you a Baskerville
-----------------------
In the cloned baskerville path run:

    cabal clean
    cabal configure
    cabal build

Running
=======
The baskerville executable will be at:

    dist/build/Baskerville/baskerville[.exe]