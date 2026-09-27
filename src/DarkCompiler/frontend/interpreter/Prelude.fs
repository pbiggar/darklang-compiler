// Prelude.fs - Type alias required by the copied interpreter syntax modules.
module Prelude

type NEList<'a> = NEList.NEList<'a>

module Map =
    let values (groups: ('k * 'v) list) : 'v list =
        groups
        |> Microsoft.FSharp.Collections.Map.ofList
        |> Microsoft.FSharp.Collections.Map.toList
        |> List.map snd
