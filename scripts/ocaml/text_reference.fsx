// text_reference.fsx - Observe frozen .NET Unicode and Int32 host behavior.
open System
open System.Text.Json
for scalar in 0 .. 0x10ffff do
    if scalar < 0xd800 || scalar > 0xdfff then
        let text = Char.ConvertFromUtf32 scalar
        let lower = text.ToLowerInvariant()
        let whitespace = text.Trim() = ""
        if lower <> text || whitespace then
            Console.WriteLine(JsonSerializer.Serialize([| box scalar; box lower; box whitespace |]))
for text in [ ""; "0"; "+1"; "-1"; " 1\t"; "1_0"; "0x10"; "2147483647";
              "2147483648"; "-2147483648"; "-2147483649"; "\u00a01";
              "1\u0000"; "1\u0000\u0000"; "1 \u0000"; "1\u0000 "; "\u00001";
              "000000000000000000000000001"; "\u000b1\u000c" ] do
    let value = match Int32.TryParse text with true, value -> box (string value) | _ -> null
    Console.WriteLine(JsonSerializer.Serialize([| box text; value |]))
