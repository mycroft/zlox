# zlox, a clox rewrite in Zig.

While reading [Crafting Interpreters](https://craftinginterpreters.com/), after having rewrote the interpreter part in Rust (see [rs-lox](https://github.com/mycroft/rs-lox)), I'm now attempting to write the bytecode compiler in Zig.

## Progression

- [x] 14 - Chunks of Bytecode
- [x] 15 - A Virtual Machine
- [x] 16 - Scanning on Demand
- [x] 17 - Compiling Expressions
- [x] 18 - Types of Values
- [x] 19 - Strings
- [x] 20 - Hash Tables
- [x] 21 - Global Variables
- [x] 22 - Local Variables
- [x] 23 - Jumping Back and Forth
- [x] 24 - Calls and Functions
- [x] 25 - Closures
- [x] 26 - Garbage Collection
- [x] 27 - Classes and Instances
- [ ] 28 - Method and Initializers
- [ ] 29 - Superclasses
- [ ] 30 - Optimization

## Build & run

There is a `justfile` to help building & running:

```sh
$ just build
$ ls -l zig-out/bin/zlox
-rwxr-xr-x 1 mycroft mycroft 2823296 Aug 25 15:36 zig-out/bin/zlox

$ set -x TRACE
$ just run
zig build run
== Hash table count:0 capacity:0 ==
== End of hash table ==

zlox> print !(5 - 4 > 3 * 2 == !nil);
== code ==
0000    1      OP_CONSTANT    0 '5'
0002    |      OP_CONSTANT    1 '4'
0004    |     OP_SUBSTRACT
0005    |      OP_CONSTANT    2 '3'
0007    |      OP_CONSTANT    3 '2'
0009    |      OP_MULTIPLY
000a    |       OP_GREATER
000b    |           OP_NIL
000c    |           OP_NOT
000d    |         OP_EQUAL
000e    |           OP_NOT
000f    |         OP_PRINT
== end of code ==

0000    1      OP_CONSTANT    0 '5'
                                [ 5 ]
0002    |      OP_CONSTANT    1 '4'
                                [ 5 ][ 4 ]
0004    |     OP_SUBSTRACT
                                [ 1 ]
0005    |      OP_CONSTANT    2 '3'
                                [ 1 ][ 3 ]
0007    |      OP_CONSTANT    3 '2'
                                [ 1 ][ 3 ][ 2 ]
0009    |      OP_MULTIPLY
                                [ 1 ][ 6 ]
000a    |       OP_GREATER
                                [ false ]
000b    |           OP_NIL
                                [ false ][ nil ]
000c    |           OP_NOT
                                [ false ][ true ]
000d    |         OP_EQUAL
                                [ false ]
000e    |           OP_NOT
                                [ true ]
000f    |         OP_PRINT
true
0010    2        OP_RETURN
== Hash table count:0 capacity:0 ==
== End of hash table ==

zlox>
```
