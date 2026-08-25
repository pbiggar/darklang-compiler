# Integer parity baseline

The integer parity baseline is the public Dark interpreter surface at
`darklang/dark` `04fbe9dcc995c6188757d583e273cbd30a3e2d3d`. The compiler
sources compared for this work are compiler HEAD
`51093e0a8e31fe45a9aa79a317fbefd6b74fbcc3` (in particular
`src/DarkCompiler/stdlib/Int.dark` through `UInt64.dark` and
`CompilerLibrary.fs:1019-1027`). The upstream executable probes are retained
under `src/Tests/e2e/upstream/stdlib/ints`.

`Int`, `Int128`, and `UInt128` are canonical decimal dynamic buffers. Their
typed conversion views are ownership-neutral; ordinary values are managed in
parameters, returns, aggregate fields, and closure captures. The signed
128-bit wrapper reduces each arithmetic result modulo 2^128 and then maps the
upper half into the signed range. The required boundary therefore holds:
`170141183460469231731687303715884105726Q + 4Q` is
`-170141183460469231731687303715884105726Q`.

The public modules use the interpreter-declared names and source-level
`Builtin.crash` behavior. Internal representation helpers remain private
implementation details. Static type enforcement remains the intentional AOT
divergence: incorrect operand and conversion types are rejected at compile
time rather than dispatched at runtime.
