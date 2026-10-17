(* AsyncSourceHooks.ml - Identify source operations that introduce transparent carriers. *)
let transportName = "Darklang.Stdlib.__Task.__transport"
let delayName = "Darklang.Stdlib.__Task.__delayTransparent"

let isProducerName name =
  List.exists
    (fun base -> String.starts_with ~prefix:(base ^ "_") name)
    [ transportName; delayName ]
