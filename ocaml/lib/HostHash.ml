(* SHA-256 over UTF-8 text for stable ownership clone names. *)
let roundConstants = [|
 0x428a2f98l;0x71374491l;0xb5c0fbcfl;0xe9b5dba5l;0x3956c25bl;0x59f111f1l;0x923f82a4l;0xab1c5ed5l;
 0xd807aa98l;0x12835b01l;0x243185bel;0x550c7dc3l;0x72be5d74l;0x80deb1fel;0x9bdc06a7l;0xc19bf174l;
 0xe49b69c1l;0xefbe4786l;0x0fc19dc6l;0x240ca1ccl;0x2de92c6fl;0x4a7484aal;0x5cb0a9dcl;0x76f988dal;
 0x983e5152l;0xa831c66dl;0xb00327c8l;0xbf597fc7l;0xc6e00bf3l;0xd5a79147l;0x06ca6351l;0x14292967l;
 0x27b70a85l;0x2e1b2138l;0x4d2c6dfcl;0x53380d13l;0x650a7354l;0x766a0abbl;0x81c2c92el;0x92722c85l;
 0xa2bfe8a1l;0xa81a664bl;0xc24b8b70l;0xc76c51a3l;0xd192e819l;0xd6990624l;0xf40e3585l;0x106aa070l;
 0x19a4c116l;0x1e376c08l;0x2748774cl;0x34b0bcb5l;0x391c0cb3l;0x4ed8aa4al;0x5b9cca4fl;0x682e6ff3l;
 0x748f82eel;0x78a5636fl;0x84c87814l;0x8cc70208l;0x90befffal;0xa4506cebl;0xbef9a3f7l;0xc67178f2l
|]
let rotate value count = Int32.logor (Int32.shift_right_logical value count) (Int32.shift_left value (32 - count))
let xor3 first second third = Int32.logxor first (Int32.logxor second third)
let small0 value = xor3 (rotate value 7) (rotate value 18) (Int32.shift_right_logical value 3)
let small1 value = xor3 (rotate value 17) (rotate value 19) (Int32.shift_right_logical value 10)
let big0 value = xor3 (rotate value 2) (rotate value 13) (rotate value 22)
let big1 value = xor3 (rotate value 6) (rotate value 11) (rotate value 25)
let sha256 bytes =
 let length = String.length bytes in
 let paddedLength = ((length + 9 + 63) / 64) * 64 in
 let padded = Bytes.make paddedLength '\000' in Bytes.blit_string bytes 0 padded 0 length; Bytes.set padded length '\128';
 let bitLength = Int64.mul (Int64.of_int length) 8L in
 for index = 0 to 7 do Bytes.set padded (paddedLength - 1 - index) (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical bitLength (index * 8)) 255L))) done;
 let hash = [|0x6a09e667l;0xbb67ae85l;0x3c6ef372l;0xa54ff53al;0x510e527fl;0x9b05688cl;0x1f83d9abl;0x5be0cd19l|] in
 let words = Array.make 64 0l in
 for block = 0 to paddedLength / 64 - 1 do
  for index = 0 to 15 do
   let start = block * 64 + index * 4 in
   let byte offset = Int32.of_int (Char.code (Bytes.get padded (start + offset))) in
   words.(index) <- Int32.logor (Int32.shift_left (byte 0) 24) (Int32.logor (Int32.shift_left (byte 1) 16) (Int32.logor (Int32.shift_left (byte 2) 8) (byte 3)))
  done;
  for index = 16 to 63 do words.(index) <- Int32.add (Int32.add (small1 words.(index - 2)) words.(index - 7)) (Int32.add (small0 words.(index - 15)) words.(index - 16)) done;
  let state = Array.copy hash in
  for index = 0 to 63 do
   let choose = Int32.logxor (Int32.logand state.(4) state.(5)) (Int32.logand (Int32.lognot state.(4)) state.(6)) in
   let majority = xor3 (Int32.logand state.(0) state.(1)) (Int32.logand state.(0) state.(2)) (Int32.logand state.(1) state.(2)) in
   let first = Int32.add (Int32.add (Int32.add state.(7) (big1 state.(4))) choose) (Int32.add roundConstants.(index) words.(index)) in
   let second = Int32.add (big0 state.(0)) majority in
   state.(7) <- state.(6); state.(6) <- state.(5); state.(5) <- state.(4); state.(4) <- Int32.add state.(3) first;
   state.(3) <- state.(2); state.(2) <- state.(1); state.(1) <- state.(0); state.(0) <- Int32.add first second
  done;
  for index = 0 to 7 do hash.(index) <- Int32.add hash.(index) state.(index) done
 done;
 Array.to_list hash |> List.map (Printf.sprintf "%08lx") |> String.concat ""
let sha256Utf8 text = sha256 (HostEncoding.utf8 text)
