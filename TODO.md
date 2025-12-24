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
- [ ] Implement negative number support in print routine
  - Add sign check in CodeGen (need ARM64 comparison or sign bit extraction)
  - Negate value if negative
  - Print '-' character before digits
- [ ] Add E2E tests for negative number operations
  - Subtraction producing negative results (e.g., `0 - 1 = -1`)
  - Negative arithmetic (e.g., `-5 + 3 = -2`)
  - Negative multiplication and division

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
- [ ] Add proper print() function to language (currently implicit)
- [ ] Support printing multiple values
- [ ] Support format strings / string interpolation

### Error Handling
- [ ] Implement proper error messages for runtime errors
- [ ] Add stderr output for errors
- [ ] Handle division by zero gracefully
- [ ] Add overflow detection and error reporting


## Code Quality

### Testing
- [ ] Add unit tests for int-to-string conversion
- [ ] Add tests for edge cases (INT_MIN, INT_MAX)
- [ ] Add tests for large numbers (> 1 million)
- [ ] Add performance benchmarks for compiler passes

### Documentation
- [ ] Document ARM64 print routine implementation
- [ ] Add examples of how to write E2E tests
- [ ] Document compiler pass pipeline


## Future Darklang Translation

When strings are implemented, the current ARM64 int-to-string routine should be
translated to Darklang. Reference implementation:

```
// Current ARM64 assembly approach (6_CodeGen.fs):
// 1. Allocate buffer
// 2. Write newline at end
// 3. Extract digits: digit = value % 10, value = value / 10
// 4. Convert digit to ASCII: '0' + digit
// 5. Store in buffer (working backwards)
// 6. Repeat until value == 0
// 7. Write buffer to stdout

// Future Darklang version (pseudocode):
let intToString (n: Int) : String =
  if n == 0 then
    "0"
  else
    let rec loop (value: Int) (acc: String) : String =
      if value == 0 then
        acc
      else
        let digit = value % 10
        let char = Char.fromCode (48 + digit)  // '0' = ASCII 48
        loop (value / 10) (String.append (String.fromChar char) acc)
    loop n ""
```
