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
    printf "$line\n" 2>&1  # stderr to avoid buffer but output on stdout
  done
}

function with-installable() {
  installable="$1"
  arg_name="$2"
  expr="$3"
  shift 3
  nix eval --impure --show-trace --apply "$arg_name: $expr" ${@} $installable
}

function with-lib() {
  expr="$1"
  shift 
  with-installable ".#lib.x86_64-linux" "lib" "$expr" ${@}
}

function eval-expr() {
  expr="$1"
  shift 1
  with-lib "$expr" --raw 2>&1 \
    | sed "s/trace: start_trace(\(.\+\)): /\\\\e[90m[\\1] \\\\e[0m/" \
    | grep -v "^trace: end_trace$" \
    | color
}

function run-tests() {
  if [[ -z "$1" ]]; then
    eval-expr "lib._tests.run {}"
  else
    eval-expr "lib.$1._tests.run {}"
  fi
}

function debug-tests() {
  if [[ -z "$1" ]]; then
    eval-expr "lib._tests.debug {}"
  else
    eval-expr "lib.$1._tests.debug {}"
  fi
}

function run-test() {
  if [[ -z "$2" ]]; then
    eval-expr "with (import <nixpkgs/lib>); concatStringsSep \"\\n\" (attrNames (lib.$1._tests.runOne))"
  else
    eval-expr "lib.$1._tests.runOne.$2 {} {}"
  fi
}

function debug-test() {
  if [[ -z "$2" ]]; then
    eval-expr "with (import <nixpkgs/lib>); concatStringsSep \"\\n\" (attrNames (lib.$1._tests.debugOne))"
  else
    eval-expr "lib.$1._tests.debugOne.$2 {} {}"
  fi
}