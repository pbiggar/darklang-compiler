(* RuntimeDataLayout.ml - Shared placement rules for writable ELF counters. *)
let elfCounterOffset dataEnd = (dataEnd + 65535) land lnot 65535
