(* HostGuid.ml - Match Guid.NewGuid's UUID v4 shape and lowercase N rendering. *)
external randomBytes : unit -> string = "dark_random_uuid_bytes"
let newGuidN () =
  let bytes = randomBytes () in
  let result = Buffer.create 32 in
  String.iteri (fun index byte ->
    let value = Char.code byte in
    let value = if index = 6 then (value land 0x0f) lor 0x40 else if index = 8 then (value land 0x3f) lor 0x80 else value in
    Buffer.add_string result (Printf.sprintf "%02x" value)) bytes;
  Buffer.contents result
