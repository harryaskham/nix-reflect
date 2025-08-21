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

function with-lib() {
  EXPR="$1"
  shift
  INSTALLABLE=".#lib.x86_64-linux"
  nix eval --impure --show-trace --apply "lib: $EXPR" ${@} $INSTALLABLE
}

function run-tests() {
  if [[ -z "$1" ]]; then
    with-lib "lib._tests.run {}" --raw
  else
    with-lib "lib.$1._tests.run {}" --raw
  fi
}

function debug-tests() {
  if [[ -z "$1" ]]; then
    with-lib "lib._tests.debug {}" --raw
  else
    with-lib "lib.$1._tests.debug {}" --raw
  fi
}

function run-test() {
    with-lib "lib.$1._tests.runOne.$2 {} {}" --raw
}

function debug-test() {
    with-lib "lib.$1._tests.debugOne.$2 {} {}" --raw
}
