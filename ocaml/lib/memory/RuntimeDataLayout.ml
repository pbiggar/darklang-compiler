(*
   RuntimeDataLayout.fs - Shared placement rules for writable ELF instrumentation.
*)
(* RuntimeDataLayout.ml - Shared placement rules for writable ELF counters. *)
(*
   ELF images use a page-aligned base. Keep writable counters off executable
   code pages, including 4/16/64 KiB host pages, so QEMU does not invalidate hot
   translated code on every instrumentation write. Normal images have no pad.
*)
let elfCounterOffset dataEnd = (dataEnd + 65535) land lnot 65535
