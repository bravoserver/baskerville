Baskerville
===========
A Minecraft server written in haskell.

Features
========

Standard Features
-----------------
* User can join and the client gracefully times out
    
Extended Features
-----------------
* Coming soon

Build
=====

Get the Haskell Platform
------------------------
**OSX and Windows**

    [http://hackage.haskell.org/platform/](http://hackage.haskell.org/platform/)

**Linux**

    Use your package manager to install haskellplatform or the link for Windows and OSX above

Build you a Baskerville
-----------------------
In the cloned baskerville path run:

    cabal clean
    cabal configure
    cabal build

Running
=======
The baskerville executable will be in:

    [cloned baskerville path]/dist/build/Baskerville/baskerville[.exe]