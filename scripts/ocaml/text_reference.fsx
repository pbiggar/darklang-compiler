// text_reference.fsx - Observe frozen .NET Unicode and Int32 host behavior.
open System
open System.Text.Json
for scalar in 0 .. 0x10ffff do
    if scalar < 0xd800 || scalar > 0xdfff then
        let text = Char.ConvertFromUtf32 scalar
        let lower = text.ToLowerInvariant()
        let whitespace = text.Trim() = ""
        let normalized = try text.Normalize() with :? ArgumentException -> null
        if lower <> text || whitespace || normalized <> text then
            Console.WriteLine(JsonSerializer.Serialize([| box scalar; box lower; box whitespace; box normalized |]))
let scalars = [0x61; 0xd; 0xa; 0; 0x301; 0x600; 0x903; 0x1100; 0x1160; 0x11a8;
               0xac00; 0xac01; 0x200d; 0xfe0f; 0x1f1e6; 0x1f1e7; 0x1f3fb; 0x1f469; 0x1f468;
               0x915; 0x94d; 0x937; 0x11f02; 0x11f36; 0x113b8; 0x113d0]
for left in scalars do
    for right in scalars do
        let text = Char.ConvertFromUtf32 left + Char.ConvertFromUtf32 right + Char.ConvertFromUtf32 left
        let iterator = Globalization.StringInfo.GetTextElementEnumerator text
        let segments = [while iterator.MoveNext() do yield iterator.GetTextElement()]
        Console.WriteLine(JsonSerializer.Serialize([| box "graphemes"; box text; box segments |]))
for unit in 0 .. 0xffff do
    let letter, digit = Char.IsLetter(char unit), Char.IsDigit(char unit)
    if letter || digit then Console.WriteLine(JsonSerializer.Serialize([| box unit; box letter; box digit |]))
for text in [ ""; "0"; "+1"; "-1"; " 1\t"; "1_0"; "0x10"; "2147483647";
              "2147483648"; "-2147483648"; "-2147483649"; "\u00a01";
              "1\u0000"; "1\u0000\u0000"; "1 \u0000"; "1\u0000 "; "\u00001";
              "000000000000000000000000001"; "\u000b1\u000c" ] do
    let value = match Int32.TryParse text with true, value -> box (string value) | _ -> null
    Console.WriteLine(JsonSerializer.Serialize([| box text; value |]))
