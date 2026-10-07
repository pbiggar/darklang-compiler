(* ParallelMoves.mli - Parallel move resolution algorithm. *)
type ('reg, 'src) moveAction = SaveToTemp of 'reg | Move of 'reg * 'src | MoveFromTemp of 'reg
val resolve : ('reg * 'src) list -> ('src -> 'reg option) -> ('reg, 'src) moveAction list
