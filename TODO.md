# TODO

## Language Features

### Strings

- [ ] Implement string type in AST
- [ ] Add string literals to parser
- [ ] Implement string operations (concatenation, length, etc.)
- [ ] Translate int-to-string routine from ARM64 assembly to Darklang
  - Current implementation: Assembly routine in `6_CodeGen.fs` (lines 118-174)
  - Converts integers to decimal strings via repeated division by 10
  - Should become a library function once strings are supported

### Negative Numbers

- [x] Implement negative number support in print routine
  - Added TBNZ instruction to test sign bit (bit 63)
  - Added NEG instruction to compute absolute value
  - Print '-' character before digits for negative numbers
- [x] Add E2E tests for negative number operations
  - Subtraction producing negative results (e.g., `0 - 1 = -1`)
  - Mixed operations with negative results (e.g., `5 - 10 + 3 = -2`)
- [ ] Add tests for negative literals and negative multiplication/division
  - Need parser support for negative literals (e.g., `-5`)
  - Tests like `-5 * 3`, `10 / -2`, `-10 * -3`, etc.

### Booleans

- [ ] Implement bool type in AST
- [ ] Add boolean literals (`true`, `false`) to parser
- [ ] Implement boolean operations (and, or, not)
- [ ] Implement comparison operators (`==`, `!=`, `<`, `>`, `<=`, `>=`)
- [ ] Add bool-to-string conversion for printing
- [ ] Add E2E tests for boolean operations

### Floats

- [ ] Implement float type in AST
- [ ] Add floating-point literals to parser
- [ ] Implement floating-point arithmetic (add, sub, mul, div)
- [ ] Add ARM64 SIMD/FP instructions for float operations
- [ ] Implement float-to-string conversion (including decimal point, exponential notation)
- [ ] Add E2E tests for floating-point operations

## Compiler Improvements

### Print Infrastructure

- [ ] Add proper polymorphic print() function to language (currently implicit)
- [ ] Support string interpolation

## Code Quality

### Testing

- [ ] Add unit tests for int-to-string conversion
- [ ] Add tests for edge cases (INT64_MIN, INT64_MAX)
- [ ] Add tests for large numbers (near 32-bit and 64-bit boundaries)

### Documentation

- [ ] Document ARM64 print routine implementation

## Future Darklang Translation

When strings are implemented, the current ARM64 int-to-string routine should be
translated to Darklang.
