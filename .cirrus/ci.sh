#!/usr/bin/env bash

set -xEeuo pipefail

setup_freebsd() {
  export GHCUP_INSTALL_BASE_PREFIX="/opt/ghc/"
  mkdir -p $HOME/.local/ghcup/bin/
  mkdir -p /opt/ghcup/bin/
  export PATH=$HOME/.local/ghcup/bin:/opt/ghcup/bin:$PATH
  curl -L https://downloads.haskell.org/~ghcup/x86_64-portbld-freebsd-ghcup --output /opt/ghcup/bin/ghcup
  chmod +x /opt/ghcup/bin/ghcup
  ghcup install ghc 9.8.2
  ghcup set ghc 9.8.2
  ghcup install cabal 3.12.1.0
  cabal update
}

configure_freebsd() {
  cabal freeze --project-file=cabal.release.project
}

build_freebsd() {
  cabal build --project-file=cabal.release.project all -j
}

test_freebsd() {
  cabal test --project-file=cabal.release.project all -j
}

help() {
  echo "Run with setup_freebsd, configure_freebsd, build_freebsd or test_freebsd."
}

case ${1:-help} in
  help) help ;;
  setup_freebsd) setup_freebsd ;;
  build_freebsd) build_freebsd ;;
  test_freebsd) test_freebsd ;;
esac
