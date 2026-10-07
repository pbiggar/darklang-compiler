(* ValueLiveness.mli - Backward value-edge transfer independent of physical storage. *)
module Make (Identity : OwnedIR.Identity) : sig
  type contract = { uses : Identity.Set.t; defines : Identity.Set.t }

  val liveBefore : contract -> Identity.Set.t -> Identity.Set.t
end
