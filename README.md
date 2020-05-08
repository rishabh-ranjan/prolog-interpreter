# rpl - An interpreter for a subset of Prolog

## Usage

	$ make
    $ ./rpl <path_to_prolog_file>

Use <code>\<enter\></code> to explore the resolution space.
Any other input starts a fresh query.
Use cancel-program key combination (Ctrl+C on \*nix systems) to exit.

## Features
Supports:
* single-line and multi-line comments ( <code>% . . .</code> and <code>/\* . . . \*/</code> respectively.
* arbitrarily nested (using optional parentheses) <code>,</code> and <code>;</code> in clause bodies and queries. <code>,</code> has a higher precedence than <code>;</code>.
* anonymous variable <code>\_</code>.
