(*
   ELF.fs - ELF Binary Format Types
   Defines data structures for the ELF (Executable and Linkable Format)
   used by Linux and other Unix-like systems.
   ELF is the standard executable format for Linux. This module defines
   the types needed to represent ELF headers, program headers, and segments.
   Structure of an ELF executable:
   - ELF Header: Identifies the file type, architecture, and entry point
   - Program Headers: Describe segments to load into memory
   - Data: The actual code and data
   Our minimal executable has:
   - ELF64 header for the selected Linux architecture
   - One PT_LOAD program header (executable code segment)
   - The machine code and optional literal data
   ELF magic number (0x7F 'E' 'L' 'F')
   ELF class (32 or 64 bit)
   ELF data encoding (endianness)
   Little-endian
   ELF version
   OS/ABI identification
   System V
   Object file type
   Executable file
   Machine architecture
   ARM 64-bit
   AMD x86-64
   Program header type
   Loadable segment
   Program header flags
   Execute
   Write
   Read
*)
(* ELF.fs - Native executable container data structures. *)
[@@@warning "-30"]
let ei_MAG0 = Char.chr (0x7F)
let ei_MAG1 = 'E'
let ei_MAG2 = 'L'
let ei_MAG3 = 'F'
let elfclass64 = Char.chr (2)
let elfdata2lsb = Char.chr (1)
let ev_CURRENT = Char.chr (1)
let elfosabi_NONE = Char.chr (0)
let et_EXEC = 2
let em_AARCH64 = 183
let em_X86_64 = 62
let pt_LOAD = 1l
let pf_X = 1l
let pf_W = 2l
let pf_R = 4l
(*
   ELF identification bytes shared by all ELF64 backends.
*)
let createIdent () = Bytes.init 16 (function 0 -> ei_MAG0 | 1 -> ei_MAG1 | 2 -> ei_MAG2 | 3 -> ei_MAG3 | 4 -> elfclass64 | 5 -> elfdata2lsb | 6 -> ev_CURRENT | 7 -> elfosabi_NONE | _ -> '\000')
(*
   ELF64 Header (64 bytes)
   16 bytes: ELF identification
   Object file type (ET_EXEC)
   Architecture (for example, EM_AARCH64 or EM_X86_64)
   ELF version
   Entry point virtual address
   Program header table file offset
   Section header table file offset (0 = none)
   Processor-specific flags
   ELF header size
   Program header entry size
   Number of program header entries
   Section header entry size
   Number of section header entries
   Section header string table index
*)
type elf64Header = {ident : bytes; typ : int; machine : int; version : int32; entry : int64; phOff : int64; shOff : int64; flags : int32; ehSize : int; phEntSize : int; phNum : int; shEntSize : int; shNum : int; shStrNdx : int}
(*
   ELF64 Program Header (56 bytes)
   Segment type (PT_LOAD)
   Segment flags (PF_R | PF_X)
   Segment file offset
   Segment virtual address
   Segment physical address (same as VAddr)
   Segment size in file
   Segment size in memory
   Segment alignment
*)
type elf64ProgramHeader = {typ : int32; flags : int32; offset : int64; vAddr : int64; pAddr : int64; fileSize : int64; memSize : int64; align : int64}
(*
   Complete ELF binary structure
   Constant string data (placed after code)
*)
type elfBinary = {header : elf64Header; programHeaders : elf64ProgramHeader list; machineCode : bytes; stringData : bytes}
