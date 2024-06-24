#!/usr/bin/env bash

case "$(uname -s)" in
        Linux*) sudo apt install upx-ucl;;
        Darwin*) brew install --build-from-source upx;;
esac
