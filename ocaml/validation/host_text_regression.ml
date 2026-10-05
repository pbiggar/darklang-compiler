(* host_text_regression.ml - Protect UTF-16 identity ordering and lookup allocation. *)
open Dark_compiler
let reference left right =
  let a=HostText.utf16Units left and b=HostText.utf16Units right in
  let rec loop i = if i=min (Array.length a) (Array.length b) then Int.compare (Array.length a) (Array.length b)
    else let order=Int.compare a.(i) b.(i) in if order=0 then loop (i+1) else order in loop 0
let sign value=Int.compare value 0
let require condition message=if not condition then failwith message
let () =
  let strings=List.map (fun units->HostText.ofUtf16Units (Array.of_list units))
    [[];[0];[65];[65;0];[0x7f];[0x80];[0x7ff];[0x800];[0xd7ff];[0xd800];[0xdbff];
     [0xdc00];[0xdfff];[0xe000];[0xffff];[0xd800;0xdc00];[0xdbff;0xdfff];
     [65;0xd800;66];[65;0xd800;0xdc00];[65;0xe000]] in
  List.iter (fun a->List.iter (fun b->require (sign (reference a b)=sign (StringOrder.compare a b)) "UTF-16 ordering differs") strings) strings;
  (* All scalar widths and BMP surrogate units, including their ordering against
     the BMP/supplementary boundary where UTF-8 byte ordering is incorrect. *)
  for scalar=0 to 0x10ffff do
    let units=if scalar<0x10000 then [|scalar|] else let n=scalar-0x10000 in [|0xd800 lor (n lsr 10);0xdc00 lor (n land 1023)|] in
    let text=HostText.ofUtf16Units units in
    List.iter (fun pivot->require (sign (reference text pivot)=sign (StringOrder.compare text pivot)) "Scalar order differs") ["";"a";"\238\128\128";"\240\144\128\128"]
  done;
  List.iter (fun malformed->List.iter (fun valid->
    let rejects a b=try ignore (StringOrder.compare a b); false with Invalid_argument message->message="Malformed UTF-8 host text" in
    require (rejects malformed valid && rejects valid malformed) "Malformed text was not rejected") ["";"a";"z"])
    ["\128";"\192\128";"\224\128\128";"\240\128\128\128";"\244\144\128\128";"\245\128\128\128";"\226\130";"a\255";"z\194a"];
  Gc.full_major ();
  let before=(Gc.quick_stat ()).Gc.minor_words in
  for _=1 to 100000 do ignore (StringOrder.compare "Stdlib.Dict.__hamtLookup" "Stdlib.Dict.__hamtInsert") done;
  require ((Gc.quick_stat ()).Gc.minor_words-.before<100.) "Identity comparison allocates per lookup";
  print_endline "UTF-16 ordering, malformed-input and allocation regressions passed"
