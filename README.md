# Confer [![Made with Haskell](https://img.shields.io/badge/Made%20in-Haskell-%235e5086?logo=haskell&style=flat-square)](https://haskell.org)  [![Configured in Lua](https://img.shields.io/badge/Configured%20in-Lua-%2300007f?logo=lua&style=flat-square)](https://www.lua.org/) ![Free Palestine](https://img.shields.io/badge/%F0%9F%94%BB-Anti_Genocide_Action-blue?style=flat&logoColor=black&labelColor=green&color=black)

Confer is a symbolic link manager that handles the deployment and synchronisation of your configuration files.

Write a configuration file to declare where your files are supposed to go, and `confer` will create the appropriate links.

## 📖 Documentation
Read the [Manual](./doc/MANUAL.md) for documentation.

For more details about the inner workings, read the [Architecture document](./doc/ARCHITECTURE.md)

## 📦 Install

### Nightly pre-releases
Pre-release binaries are available for the following platforms:

* [Linux-x86_64-musl (statically linked)](https://github.com/tchoutri/confer/releases/download/confer-head/confer-head-Linux-static-x86_64.tar.gz)
* [Linux-x86_64-glibc (dynamically linked)](https://github.com/tchoutri/confer/releases/download/confer-head/confer-head-Linux-x86_64.tar.gz)
* [macOS-arm64](https://github.com/tchoutri/confer/releases/download/confer-head/confer-head-macOS-arm64.tar.gz)

## 🔧 Build

*Confer* is made in Haskell. To build it from source, use [ghcup](https://www.haskell.org/ghcup/) to install the following toolchains:
* `cabal` 3.10
* `ghc` 9.8.2
