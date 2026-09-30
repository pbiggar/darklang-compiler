// LiteralPool.fs - Dense literal storage for late constant resolution.
// Pools are frozen once in first-use order; reverse indexes deduplicate values.

module LiteralPool

type StringPool = {
    Strings: (string * int) array
    StringToId: Map<string, int>
}

/// Exact IEEE-754 bits distinguish signed zero and NaN payloads.
type FloatPool = {
    Floats: float array
    FloatBitsToId: Map<int64, int>
}

let emptyStringPool : StringPool = {
    Strings = [||]
    StringToId = Map.empty
}

let emptyFloatPool : FloatPool = {
    Floats = [||]
    FloatBitsToId = Map.empty
}

/// Build in first-use order without copying a growing array for every literal.
let createStringPool (values: seq<string>) : StringPool =
    let entries, ids, _ =
        values
        |> Seq.fold (fun (entries, ids, next) value ->
            if Map.containsKey value ids then entries, ids, next
            else
                let length = System.Text.Encoding.UTF8.GetByteCount value
                (value, length) :: entries, Map.add value next ids, next + 1)
            ([], Map.empty, 0)
    { Strings = entries |> List.rev |> List.toArray; StringToId = ids }

let createFloatPool (values: seq<float>) : FloatPool =
    let entries, ids, _ =
        values
        |> Seq.fold (fun (entries, ids, next) value ->
            let bits = System.BitConverter.DoubleToInt64Bits value
            if Map.containsKey bits ids then entries, ids, next
            else value :: entries, Map.add bits next ids, next + 1)
            ([], Map.empty, 0)
    { Floats = entries |> List.rev |> List.toArray; FloatBitsToId = ids }
