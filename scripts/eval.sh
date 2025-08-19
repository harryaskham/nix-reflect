#!/usr/bin/env bash

function with-lib() {
  EXPR="$1"
  shift
  nix eval --show-trace --apply "lib: $EXPR" $@ .#lib.x86_64-linux
}

function run-tests() {
  if [[ -z "$1" ]]; then
    with-lib "lib._tests.run {}"
  else
    with-lib "lib.$1._tests.run {}"
  fi
}