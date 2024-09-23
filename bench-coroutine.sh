#!/bin/sh
cabal run speff-bench -- --svg bench-coroutine.svg --pattern '$2 == "coroutine"'
