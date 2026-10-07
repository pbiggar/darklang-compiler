(* ELF.mli - Native executable container data structures. *)
[@@@warning "-30"]
val ei_MAG0 : char
val ei_MAG1 : char
val ei_MAG2 : char
val ei_MAG3 : char
val elfclass64 : char
val elfdata2lsb : char
val ev_CURRENT : char
val elfosabi_NONE : char
val et_EXEC : int
val em_AARCH64 : int
val em_X86_64 : int
val pt_LOAD : int32
val pf_X : int32
val pf_W : int32
val pf_R : int32
val createIdent : unit -> bytes
type elf64Header = {ident : bytes; typ : int; machine : int; version : int32; entry : int64; phOff : int64; shOff : int64; flags : int32; ehSize : int; phEntSize : int; phNum : int; shEntSize : int; shNum : int; shStrNdx : int}
type elf64ProgramHeader = {typ : int32; flags : int32; offset : int64; vAddr : int64; pAddr : int64; fileSize : int64; memSize : int64; align : int64}
type elfBinary = {header : elf64Header; programHeaders : elf64ProgramHeader list; machineCode : bytes; stringData : bytes}
