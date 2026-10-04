(* Typed matching of the frozen fixture regex alphabet, with UTF-16 captures. *)
type characterClass=Dot | Digit | Space | Characters of string
type token=Literal of string | Repeat of characterClass * int * bool | Capture of token list | Alternatives of token list list
val matched : token list -> string -> string array option
val literal : string -> token
val space : token
val spaces : token
val digits : token
val any : token
val integer : int -> bool -> string -> Z.t option
val int64 : string -> int64 option
