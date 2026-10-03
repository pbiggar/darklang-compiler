(* Emit complete migration observations without a buffer proportional to the wire row. *)
val to_channel : out_channel -> Yojson.Basic.t -> unit
