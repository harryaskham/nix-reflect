# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Testing
- `./test.sh` - Enter development shell and run tests
- `nix develop .#test` - Enter development shell directly
- `run-tests` - Run all tests (available in dev shell via scripts/eval.sh)
- `run-tests parser` - Run parser-specific tests
- `run-tests eval` - Run evaluator-specific tests
- `with-lib "lib.parser.parse \"1 + 1\""` - Evaluate expressions using the library

### Development Shell
The project uses a Nix flake development environment:
- `nix develop` enters the default development shell
- Shell automatically sources `./scripts/eval.sh` which provides helper functions
- `with-lib` function evaluates expressions against the library: `with-lib "EXPR"`

## Architecture Overview

nix-reflect is a pure-Nix library for parsing, evaluating, and reflecting on Nix expressions. It consists of three main components:

### Core Modules

**Parser (`lib/parser/`)**: 
- Pure Nix parser built on `nix-parsec`
- Converts Nix source code into AST nodes
- Comprehensive support for all Nix language constructs
- Main entry: `parser.parse "expression"` returns AST

**Evaluator (`lib/eval/`)**: 
- AST interpreter that reduces parsed expressions to values
- Multiple evaluation strategies: AST-based (`eval.ast`) and store-based (`eval.store`)
- Monadic evaluation with error handling via Either types
- Main entry: `eval "expression"` returns `Right value` or `Left error`

**Function Library (`lib/eval/fn.nix`)**:
- Creates callable lambda functions from string expressions
- Supports text transformation via `mapText` and AST transformation via `mapAST`
- Usage: `eval.fn "x: x + 1"` creates a callable function

### Key Features

**AST Manipulation**: All AST nodes support `.mapNode` for transformation and `.fmap` for type-preserving mapping.

**Source Preservation**: Parsed expressions retain original source text in `__src` attribute for debugging.

**Reflection Capabilities**: The library can parse and evaluate its own source code, enabling meta-programming.

**Test Framework Integration**: Uses collective-lib test framework with comprehensive test suites for all components.

### File Structure
- `lib/default.nix` - Main library entry point, assembles all modules
- `lib/parser/default.nix` - Complete Nix parser implementation
- `lib/eval/default.nix` - Evaluation module dispatcher 
- `lib/eval/ast.nix` - AST-based evaluator
- `lib/eval/store.nix` - Store-based evaluator (uses derivations)
- `lib/eval/fn.nix` - Function-from-string utilities
- `lib/eval/monad.nix` - Evaluation monad and state management
- `examples/expr.nix` - Sample Nix expression for testing

### Dependencies
- Built on `nix-parsec` for parser combinators
- Uses `collective-public` library for utilities and testing framework
- Pure Nix implementation with no external build tools required

### Testing Philosophy
- Comprehensive test coverage for parser and evaluator
- Tests use `expect.eq` and `expect.eqOn` for assertions
- Parser tests validate AST structure with source text preservation
- Evaluator tests verify correct reduction of complex expressions