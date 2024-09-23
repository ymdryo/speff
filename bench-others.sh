#!/bin/sh
cabal run speff-bench -- --svg bench-others.svg --pattern '$2 != "coroutine"'
