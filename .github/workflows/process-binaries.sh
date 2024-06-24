#!/usr/bin/env bash

case "$(uname -s)" in
        Linux*) strip $CONFER_EXEC && upx -9 $CONFER_EXEC
        Darwin*) strip $CONFER_EXEC;;
