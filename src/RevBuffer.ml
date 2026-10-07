(* RevBuffer.ml - Append in constant time and retain source order at boundaries. *)
type 'a t = { mutable reversed : 'a list; mutable length : int }

let create () = { reversed = []; length = 0 }

let add buffer value =
  buffer.reversed <- value :: buffer.reversed;
  buffer.length <- buffer.length + 1

let toList buffer = List.rev buffer.reversed
let length buffer = buffer.length

let last buffer =
  match buffer.reversed with [] -> None | value :: _ -> Some value
