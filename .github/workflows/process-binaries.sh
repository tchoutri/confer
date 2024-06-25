#!/usr/bin/env bash

CONFER_EXEC=$(cabal list-bin --project-file=cabal.release.project confer:exe:confer)

ldd $CONFER_EXEC

case "$(uname -s)" in
        Linux*) strip $CONFER_EXEC && upx -9 $CONFER_EXEC;;
        Darwin*) strip $CONFER_EXEC;;
esac
