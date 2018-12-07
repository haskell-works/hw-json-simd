#!/usr/bin/env bash

CABAL_FLAGS="-j8"

cmd="$1"

shift

case "$cmd" in
  build)
    cabal new-build \
      $CABAL_FLAGS "$@"
    ;;

  test)
    cabal new-test \
      $CABAL_FLAGS "$@"
    ;;
esac
