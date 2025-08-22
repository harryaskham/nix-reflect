#!/usr/bin/env bash

function maybe-install-nix() {
  if ! which nix; then
    sh <(curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install) --no-daemon
    . /home/ubuntu/.nix-profile/etc/profile.d/nix.sh
  fi
}

function maybe-bootstrap-cursor-agent() {
  if [[ "$(hostname)" == "cursor" ]]; then
    maybe-install-nix
  fi
}

function color() {
  IFS=''
  while read -r line ; do
    printf "$line\n"
  done
}

function with-lib() {
  EXPR="$1"
  shift
  INSTALLABLE=".#lib.x86_64-linux"
  nix eval --impure --show-trace --apply "lib: $EXPR" ${@} $INSTALLABLE
}

function eval-test-expr() {
  with-lib "$1" --raw 2>&1 \
    | sed "s/trace: start_trace(\(.\+\)): /\\\\e[90m[\\1] \\\\e[0m/" \
    | grep -v "^trace: end_trace$" \
    | color
}

function run-tests() {
  if [[ -z "$1" ]]; then
    eval-test-expr "lib._tests.run {}"
  else
    eval-test-expr "lib.$1._tests.run {}"
  fi
}

function debug-tests() {
  if [[ -z "$1" ]]; then
    eval-test-expr "lib._tests.debug {}"
  else
    eval-test-expr "lib.$1._tests.debug {}"
  fi
}

function run-test() {
  if [[ -z "$2" ]]; then
    eval-test-expr "with (import <nixpkgs/lib>); concatStringsSep \"\\n\" (attrNames (lib.$1._tests.runOne))"
  else
    eval-test-expr "lib.$1._tests.runOne.$2 {} {}"
  fi
}

function debug-test() {
  if [[ -z "$2" ]]; then
    eval-test-expr "with (import <nixpkgs/lib>); concatStringsSep \"\\n\" (attrNames (lib.$1._tests.debugOne))"
  else
    eval-test-expr "lib.$1._tests.debugOne.$2 {} {}"
  fi
}
