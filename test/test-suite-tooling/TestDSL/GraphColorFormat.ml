(* GraphColorFormat.fs - Parser for graph-coloring algorithm fixtures.
   Represents graph topology, coloring preferences, and observable properties as typed data. *)
open Dark_compiler
module M=StringOrder.Map
let (let*)=Result.bind
type countExpectation=Exactly of int|AtMost of int|AtLeast of int
type graphColorTest={name:string;vertices:int list;edges:(int*int) list;availableColors:int;precolored:(int*int) list;preferencePairs:(int*int) list;movePairs:(int*int) list;expectedChromatic:countExpectation option;expectedSpills:countExpectation option;expectedColored:countExpectation option;expectedColors:(int*int) list;expectedSame:(int*int) list;expectedDifferent:(int*int) list;expectMcsCoversAll:bool;expectedSelectionChecks:int option;sourceFile:string}
let knownSections=["NAME";"VERTICES";"EDGES";"AVAILABLE-COLORS";"PRECOLORED";"PREFER";"MOVE-PREFER";"EXPECT-CHROMATIC";"EXPECT-SPILLS";"EXPECT-COLORED";"EXPECT-COLORS";"EXPECT-SAME";"EXPECT-DIFFERENT";"EXPECT-MCS-ORDERING";"EXPECT-SELECTION-CHECKS"]
let groupCases sections=
 let rec loop completed current=function
 |[]->Ok (List.rev (if current=[] then completed else List.rev current::completed))
 |(("NAME",_) as section)::rest->loop (if current=[] then completed else List.rev current::completed) [section] rest
 |section::rest->if current=[] then Error ("Graph-color case must start with NAME, found "^fst section) else loop completed (section::current) rest in
 loop [] [] sections
let toSectionMap sections=
 match List.find_opt (fun (name,_)->not (List.mem name knownSections)) sections with
 |Some (name,_)->Error ("Unknown graph-color section: "^name)
 |None->match List.find_opt (fun (name,_)->List.length (List.filter (fun (other,_)->name=other) sections)>1) sections with
 |Some (name,_)->Error ("Duplicate graph-color section: "^name)
 |None->Ok (List.fold_left (fun map (key,value)->M.add key value map) M.empty sections)
let required name values=match M.find_opt name values with
 |Some value when HostText.trim value<>""->Ok (HostText.trim value)
 |Some _->Error ("Graph-color section "^name^" cannot be empty")
 |None->Error ("Missing required graph-color section: "^name)
let optional name values=Option.map HostText.trim (M.find_opt name values)
let parseInt description text=match HostText.tryParseInt32 (HostText.trim text) with
 |Some value when value>=0l->Ok (Int32.to_int value)
 |Some _|None->Error ("Invalid "^description^" '"^HostText.trim text^"' (expected non-negative integer)")
let tokens text=
 let buffer=Buffer.create (String.length text) in String.iter (fun c->Buffer.add_char buffer (if String.contains " \t\n\r," c then ' ' else c)) text;
 String.split_on_char ' ' (Buffer.contents buffer) |> List.filter (fun token->token<>"")
let parseVertices text=
 if HostText.lowerInvariant (HostText.trim text)="none" then Ok [] else
 let* vertices=ResultList.traverse (parseInt "vertex") (tokens text) in
 if List.length (List.sort_uniq Int.compare vertices)=List.length vertices then Ok vertices else Error "VERTICES contains a duplicate vertex"
let parsePair separator description token=
 let parts=String.split_on_char separator token in
 match parts with
 |[left;right]->let* left=parseInt (description^" left value") left in let* right=parseInt (description^" right value") right in Ok (left,right)
 |[]|[_]|_::_::_->Error ("Invalid "^description^" '"^token^"' (expected A"^String.make 1 separator^"B)")
let parsePairs separator description text=
 if HostText.lowerInvariant (HostText.trim text)="none" then Ok [] else
 let* pairs=ResultList.traverse (parsePair separator description) (tokens text) in
 if List.length (List.sort_uniq Stdlib.compare pairs)=List.length pairs then Ok pairs else Error (description^" contains a duplicate pair")
let parseOptionalPairs separator description section values=match optional section values with None->Ok []|Some text->parsePairs separator description text
let parseCount description text=
 let text=HostText.trim text in
 let constructor,value=if String.starts_with ~prefix:"<=" text then (fun x->AtMost x),String.sub text 2 (String.length text-2) else if String.starts_with ~prefix:">=" text then (fun x->AtLeast x),String.sub text 2 (String.length text-2) else (fun x->Exactly x),text in
 Result.map constructor (parseInt description value)
let parseOptionalCount description section values=match optional section values with None->Ok None|Some text->Result.map Option.some (parseCount description text)
let pairVertices pairs=List.concat_map (fun (left,right)->[left;right]) pairs
let validateKnownVertices vertices description pairs=match List.find_opt (fun vertex->not (List.mem vertex vertices)) (pairVertices pairs) with
 |Some vertex->Error (Printf.sprintf "%s references unknown vertex %d" description vertex)|None->Ok ()
let validateKnownFirstVertices vertices description pairs=match List.find_opt (fun (vertex,_)->not (List.mem vertex vertices)) pairs with
 |Some (vertex,_)->Error (Printf.sprintf "%s references unknown vertex %d" description vertex)|None->Ok ()
let parseCase path sections=
 let* values=toSectionMap sections in
 let* name=required "NAME" values in let* verticesText=required "VERTICES" values in let* colorsText=required "AVAILABLE-COLORS" values in
 let* vertices=parseVertices verticesText in let* availableColors=parseInt "available color count" colorsText in
 let* edges=parseOptionalPairs '-' "EDGES" "EDGES" values in
 let* precolored=parseOptionalPairs '=' "PRECOLORED" "PRECOLORED" values in
 let* preferencePairs=parseOptionalPairs '-' "PREFER" "PREFER" values in
 let* movePairs=parseOptionalPairs '-' "MOVE-PREFER" "MOVE-PREFER" values in
 let* expectedColors=parseOptionalPairs '=' "EXPECT-COLORS" "EXPECT-COLORS" values in
 let* expectedSame=parseOptionalPairs '-' "EXPECT-SAME" "EXPECT-SAME" values in
 let* expectedDifferent=parseOptionalPairs '-' "EXPECT-DIFFERENT" "EXPECT-DIFFERENT" values in
 let* expectedChromatic=parseOptionalCount "chromatic expectation" "EXPECT-CHROMATIC" values in
 let* expectedSpills=parseOptionalCount "spill expectation" "EXPECT-SPILLS" values in
 let* expectedColored=parseOptionalCount "colored expectation" "EXPECT-COLORED" values in
 let* expectedSelectionChecks=match optional "EXPECT-SELECTION-CHECKS" values with None->Ok None|Some text->Result.map Option.some (parseInt "selection check expectation" text) in
 let* expectMcsCoversAll=match optional "EXPECT-MCS-ORDERING" values with None->Ok false|Some text when HostText.lowerInvariant text="all"->Ok true|Some text->Error ("Invalid EXPECT-MCS-ORDERING '"^text^"' (expected 'all')") in
 let* _=ResultList.traverse (fun (description,pairs)->validateKnownVertices vertices description pairs) ["EDGES",edges;"PREFER",preferencePairs;"MOVE-PREFER",movePairs;"EXPECT-SAME",expectedSame;"EXPECT-DIFFERENT",expectedDifferent] in
 let* _=ResultList.traverse (fun (description,pairs)->validateKnownFirstVertices vertices description pairs) ["PRECOLORED",precolored;"EXPECT-COLORS",expectedColors] in
 let hasExpectation=Option.is_some expectedChromatic || Option.is_some expectedSpills || Option.is_some expectedColored || expectedColors<>[] || expectedSame<>[] || expectedDifferent<>[] || expectMcsCoversAll || Option.is_some expectedSelectionChecks in
 if not hasExpectation then Error "Graph-color case requires at least one EXPECT section"
 else if availableColors=0 && vertices<>[] then Error "AVAILABLE-COLORS must be positive for a non-empty graph"
 else if List.exists (fun (_,color)->color>=availableColors) precolored then Error "PRECOLORED contains a color outside AVAILABLE-COLORS"
 else Ok {name;vertices;edges;availableColors;precolored;preferencePairs;movePairs;expectedChromatic;expectedSpills;expectedColored;expectedColors;expectedSame;expectedDifferent;expectMcsCoversAll;expectedSelectionChecks;sourceFile=path}
let parseGraphColorFileContent path content=
 let sections=Common.parseSections (Common.normalizeLineEndings content) in
 if sections=[] then Error "Graph-color fixture contains no sections" else let* cases=groupCases sections in ResultList.traverse (parseCase path) cases
