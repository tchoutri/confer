# Architecture 

## Logical Components

### CLI

The CLI is the main interaction point with *confer*. 

### Symlink Effect

The Symlink effect is the abstraction layer that implements the business logic
and enforces its invariants at all time. It declares the operations that 
are performed on symbolic links, and provides two interpreters for them:
  * The `FileSystem` interpreter is the *effectful* interpreter that will modify
  your file system. 
  * The `Pure` interpreter does not modify the file system. It is used when the
  user calls the programw with `--dry-run` and will only perform sanity checks

## Lua Runtime 

### Lua Evaluator

*Confer* embeds the Lua runtime to evaluate the user configuration.
All standard libraries of the Lua 5.4 interpreter are made available.

### Lua APIs

*Confer* provides its own APIs to query the host machine's information and the current 
users's information, like the home directory.

The `confer` namespace is implemented in pure Lua. It is distributed with `confer`.
The `host` and `user` namespaces are implemented in Haskell and made available 
to the Lua environment.
