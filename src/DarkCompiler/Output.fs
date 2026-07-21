// Output.fs - Output helper functions
//
// Provides simple print functions for stdout and stderr.
// These functions use string interpolation and handle newlines explicitly.

module Output

open System

/// Print to stdout without newline
let print (s: string) : unit =
    Console.Write(s)

/// Print to stdout with newline
let println (s: string) : unit =
    Console.WriteLine(s)

/// Print to stderr without newline
let eprint (s: string) : unit =
    Console.Error.Write(s)

/// Print to stderr with newline
let eprintln (s: string) : unit =
    Console.Error.WriteLine(s)
