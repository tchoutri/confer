#!/usr/bin/env bash

set -xEeuo pipefail

build_freebsd() {
  export GHCUP_INSTALL_BASE_PREFIX="/opt/ghc/"
  mkdir -p ~/.local/ghcup/bin/
  curl -L https://downloads.haskell.org/~ghcup/x86_64-portbld-freebsd-ghcup --output /opt/ghcup/bin/ghcup
  chmod +x /opt/ghcup/bin/ghcup
  ghcup install ghc 9.10.3
  ghcup set ghc 9.10.3
  ghcup install cabal 3.14.2.0
}

help() {
  echo "Run with \`build_freebsd\`"
}

case ${1:-help} in
  help) help ;;
  build_frebsd) build_freebsd ;;

esac
