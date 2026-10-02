// float_reference.fsx - Frozen .NET binary64 R-format observations.
open System
open System.Globalization
open System.Text.Json
let observe (bits: uint64) =
    let value = BitConverter.Int64BitsToDouble(int64 bits)
    Console.WriteLine(JsonSerializer.Serialize([|bits.ToString("x16");value.ToString("R",CultureInfo.InvariantCulture)|]))
for bits in [0UL; 0x8000000000000000UL; 1UL; 0x7fffffffffffffffUL; UInt64.MaxValue; 0x7ff0000000000000UL; 0xfff0000000000000UL] do observe bits
for value in [0.1; 1e-4; 1e-5; 1e16; 1e17; 1e18; 1.2345678901234567; 1e-300; 1e300] do observe (uint64 (BitConverter.DoubleToInt64Bits value))
let rec probes count bits =
    if count > 0 then
        let bits = bits ^^^ (bits <<< 13)
        let bits = bits ^^^ (bits >>> 7)
        let bits = bits ^^^ (bits <<< 17)
        observe bits
        probes (count - 1) bits
probes 10000 0x9e3779b97f4a7c15UL
