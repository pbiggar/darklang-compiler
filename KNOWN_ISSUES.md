# Known Issues

No known issues at this time.

## Recently Fixed

### Register Spilling Segfault (Fixed)

**Root Cause**: Stack frame layout collision between spill slots and callee-saved registers.

The prologue was allocating callee-saved space before spill space, causing spill slots at FP-8, FP-16, etc. to collide with the callee-saved register save area.

**Fix**: Reordered prologue/epilogue in `src/DarkCompiler/passes/6_CodeGen.fs` to allocate spill space before callee-saved space, ensuring spill slots are immediately below FP.
