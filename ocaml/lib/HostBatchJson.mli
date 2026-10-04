(* Decode labeled batch manifests with native JSON token locations. *)
type item={kind:string;name:string;source:string;output:string}
val parse : string -> item option list option
