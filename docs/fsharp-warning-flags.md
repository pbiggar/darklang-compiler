# F# Warning/Error Flags Evaluation

## Current Configuration

Both `DarkCompiler.fsproj` and `Tests.fsproj` have:
- `<TreatWarningsAsErrors>true</TreatWarningsAsErrors>` - All warnings are errors
- `<NoWarn>NU1900</NoWarn>` - Suppresses NuGet package vulnerability warnings

## Status

The codebase currently compiles with **0 warnings**. This is the ideal state.

## Potential Additional Flags

### Already Effective (via TreatWarningsAsErrors)

Since `TreatWarningsAsErrors` is enabled, all standard F# warnings are already treated as errors. This includes:

- **FS0025** - Incomplete pattern matches
- **FS0026** - Incomplete rule (pattern match guard)
- **FS0020** - Value discarded (expression result unused)
- **FS0058** - Possible incorrect indentation

### Optional Stricter Flags to Consider

1. **`--warnon:1182`** - Warn on unused variables
   - Status: Not recommended - can be noisy in pattern matches

2. **`--warnon:3390`** - Warn about XML doc problems
   - Status: Not relevant - codebase doesn't use XML docs

3. **`--nowarn:3391`** - Suppress "this expression is a function" warnings
   - Status: Not needed - no such warnings present

## Recommendations

**No changes recommended.** The current configuration is optimal:

1. `TreatWarningsAsErrors=true` catches all important issues
2. Zero warnings in the codebase
3. The only suppression (`NU1900`) is appropriate for development

## If Issues Arise

If specific warnings become problematic, prefer fixing the code rather than
broadening `<NoWarn>`. If a transition really needs a temporary suppression,
keep it narrow, document why it exists, and include the condition for removing
it.
