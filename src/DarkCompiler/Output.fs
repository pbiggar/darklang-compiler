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
/// TODO: Replace with Darklang error handling during language migration.
let crash (message: string) : 'a =
    failwith message
