(*
   Immediates.fs - Materialize integer constants for ARM64 runtime instruction generators.
*)
let generateLoadUInt64Immediate (dest:ARM64.reg) value =
 let chunk shift=Int64.to_int (Int64.logand (Int64.shift_right_logical value shift) 0xffffL) in
 let chunks=[chunk 0,0;chunk 16,16;chunk 32,32;chunk 48,48] in
 match List.find_opt (fun (value,_) -> value<>0) chunks with
 | None -> [ARM64.MOVZ (dest,0,0)]
 | Some (firstValue,firstShift) ->
   let movkInstrs=List.filter_map (fun (value,shift) -> if shift=firstShift || value=0 then None else Some (ARM64.MOVK (dest,value,shift))) chunks in
   ARM64.MOVZ (dest,firstValue,firstShift)::movkInstrs
let generateLoadNonNegativeIntImmediate dest value =
 if value<0 then Crash.crash (Printf.sprintf "Runtime: cannot load negative unsigned immediate %d" value)
 else generateLoadUInt64Immediate dest (Int64.of_int value)
