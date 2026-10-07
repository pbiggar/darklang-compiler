(*
   Reachability is not permission to reuse storage: aliases, destruction, and
   the storage-specific ownership contract must be resolved independently.
*)
(* ValueLiveness.fs - Backward value-edge transfer independent of physical storage. *)
module Make (Identity : OwnedIR.Identity) = struct
 type contract = {uses : Identity.Set.t; defines : Identity.Set.t}
 let liveBefore contract liveAfter = Identity.Set.union contract.uses (Identity.Set.diff liveAfter contract.defines)
end
