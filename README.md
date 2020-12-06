# The LFPL Interpreter

Please see `report.pdf` for more details. 
This document describes how to compile and run the interpreter.
Note that a pre-built binary `lfpl` has already been provided.

Compilation requires [Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
Once `stack` is available on `$PATH`, compilation just requires:

```sh
% stack build
```

This will create the binary `lfpl`. The newly compiled version can be 
run with `stack run -- <program args>`. You can run `stack run --` to view a usage message.

Some examples have been placed in the `test/` directory. 

```sh
% cd test/
% stack run -- badappend.lfpl
badappend.lfpl: 6:57 - 6:58: Variable 'd' of type '<>' is not heap-free and cannot be used multiple times
Last usage: badappend.lfpl: 6:46 - 6:47
Declared at: badappend.lfpl: 4:13 - 8:1

% stack run -- isort.lfpl "[1, 6, 2, 7, -32, 15819]: int"
[-32, 1, 2, 6, 7, 15819]: (int) list
```
