#!/usr/bin/env bash

CONFER_PATH="distribution/confer"

ldd $CONFER_PATH
file $CONFER_PATH

case "$(uname -s)" in
        Linux*) upx -9 $CONFER_PATH;;
        Darwin*) echo "upx crashes on macOS Ventura and above" ;;
esac
