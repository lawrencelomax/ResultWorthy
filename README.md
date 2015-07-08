ResultWorthy
==============

A half-working parser of Swift (pre-2.0) functions and types in Haskell. Parses function signatures, declarations, typealiases, classes, structures.
Provides a proto-AST of the declarations and structure of types, without parsing function bodies. This should make it possible to code generate, for example to generate trampoline functions.

Notes:
  - This is my first significant Haskell experience, so expect hilarity.
  - There will be ways of easily breaking the parser.
  - Build with `cabal install`
  - For example run with `./dist/build/ResultWorthy/ResultWorthy --input "ResultWorthy/Tests/Fixtures/FoundationHeader.swift" --tree`
  - Canonical example of code generation in (pre-2.0) Swift is to transform `A -> NSErrorPointer -> B` to `A -> Result<B>` from many of the places where `NSErrorPointer` is provided as the last argument in Cocoa APIs.
