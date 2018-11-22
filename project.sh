#!/usr/bin/env bash

STACK_FLAGS="
  --flag bits-extra:bmi2
  --flag hw-rankselect-base:bmi2
  --flag hw-rankselect:bmi2
"

cmd="$1"

shift

case "$cmd" in
  install)
    stack install \
      --test --no-run-tests --bench --no-run-benchmarks \
      $STACK_FLAGS "$@"
    ;;

  build)
    stack build \
      --test --no-run-tests --bench --no-run-benchmarks \
      $STACK_FLAGS "$@"
    ;;

  test)
    stack test \
      $STACK_FLAGS "$@"
    ;;

  exec)
    stack exec \
      $STACK_FLAGS "$@"
    ;;

  bench)
    stack bench \
      $STACK_FLAGS "$@"
    ;;

  repl)
    stack repl \
      $STACK_FLAGS "$@"
    ;;
esac
