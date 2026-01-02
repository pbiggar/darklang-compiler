// Output.fs - Output helper functions
//
// Provides simple print functions for stdout and stderr.
// These functions use string interpolation and handle newlines explicitly.

module Output

/// Print to stdout without newline
let print (s: string) : unit =
    printf "%s" s

/// Print to stdout with newline
let println (s: string) : unit =
    printf "%s\n" s

/// Print to stderr without newline
let eprint (s: string) : unit =
    eprintf "%s" s

/// Print to stderr with newline
let eprintln (s: string) : unit =
    eprintf "%s\n" s

/// Crash the program with an error message.
/// Used for internal invariant violations (unreachable code).
///
/// When the compiler is migrated to Darklang (self-hosting), this will
/// be replaced with Darklang's error handling (Result types or similar).
let crash (message: string) : 'a =
    failwith message
