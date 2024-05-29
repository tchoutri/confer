# Confer [![Made with Haskell](https://img.shields.io/badge/Made%20in-Haskell-%235e5086?logo=haskell&style=flat-square)](https://haskell.org)  [![Configured in Lua](https://img.shields.io/badge/Configured%20in-Lua-%2300007f?logo=lua&style=flat-square)](https://www.lua.org/)

> The dotfiles manager

## 📖 Documentation
Read the [Manual](./doc/MANUAL.md) for documentation.

For more details about the inner workings, read the [Architecture document](./doc/ARCHITECTURE.md)

## 📦 Install

No binaries are currently available. See the Build instructions

## 🔧 Build

*Confer* is made in Haskell. To build it from source, use [ghcup](https://www.haskell.org/ghcup/) to install the following toolchains:
* `cabal` 3.12
    `$ ghcup install cabal --force -u https://github.com/haskell/cabal/releases/download/cabal-head/cabal-head-Linux-x86_64.tar.gz head`
* `ghc` 9.10.1
    `$ ghcup tui` -> select GHC 9.10.1
