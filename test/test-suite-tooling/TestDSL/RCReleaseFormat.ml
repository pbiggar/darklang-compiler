(* RCReleaseFormat.fs - Parser for semantic reference-release fixtures.
   Describes canonical managed object graphs without exposing their LIR layout. *)
[@@@warning "-4-42"]
open Dark_compiler
module M=StringOrder.Map
let (let*)=Result.bind
type managedShape=Int64Value|EnumValue|DynamicString|LiteralString|DynamicBlob|ListValue of managedShape|DictValue of managedShape*managedShape|TupleValue of managedShape list|RecordValue of managedShape list|SumValue of managedShape|ClosureValue of managedShape list
type preservedRegister={register:LIR.physReg;value:int64}
type rootPlacement=CanonicalRoot|ExplicitRoot of LIR.physReg*preservedRegister list
type rCReleaseTest={name:string;root:managedShape;placement:rootPlacement;sourceFile:string}
type shapeToken=Identifier of string|LeftParen|RightParen|Comma
let knownSections=StringOrder.Set.of_list ["NAME";"ROOT";"ROOT-REGISTER";"PRESERVE"]
let tokenizeShape source=
 let units=Text.scalars source in let count=Array.length units in
 let identifierChar c=Text.isLetter c || Text.isDigit c || c=45 || c=95 in
 let whitespace c=Uchar.is_valid c && Uucp.White.is_white_space (Uchar.of_int c) in
 let rec loop offset tokens=if offset=count then Ok (List.rev tokens) else
 let c=units.(offset) in if whitespace c then loop (offset+1) tokens else
 match c with 40->loop (offset+1) (LeftParen::tokens)|41->loop (offset+1) (RightParen::tokens)|44->loop (offset+1) (Comma::tokens)|_ when identifierChar c->
 let ending=ref offset in while !ending<count && identifierChar units.(!ending) do incr ending done;
 let text=Text.ofScalars (Array.sub units offset (!ending-offset)) in loop !ending (Identifier text::tokens)
 |_->Error ("Invalid ROOT character '"^Text.ofScalars [|c|]^"' at offset "^string_of_int offset) in loop 0 []
let parseShape source=
 let rec parseOne tokens=
 let parseArguments allowEmpty remaining=
  let rec loop parsed rest=match rest with RightParen::tail when allowEmpty || parsed<>[]->Ok (List.rev parsed,tail)|RightParen::_->Error "Managed shape requires at least one argument"|_->
   let* value,after=parseOne rest in match after with Comma::tail->loop (value::parsed) tail|RightParen::tail->Ok (List.rev (value::parsed),tail)|_->Error "Expected ',' or ')' in managed shape" in loop [] remaining in
 match tokens with Identifier name::rest->(match Text.lowerInvariant name,rest with
 |"i64",tail->Ok (Int64Value,tail)|"enum",tail->Ok (EnumValue,tail)|"string",tail->Ok (DynamicString,tail)|"literal-string",tail->Ok (LiteralString,tail)|"blob",tail->Ok (DynamicBlob,tail)
 |"list",LeftParen::tail->let* shapes,remaining=parseArguments false tail in (match shapes with [shape]->Ok (ListValue shape,remaining)|_->Error "list requires exactly one argument")
 |"dict",LeftParen::tail->let* shapes,remaining=parseArguments false tail in (match shapes with [key;value]->Ok (DictValue (key,value),remaining)|_->Error "dict requires exactly two arguments")
 |"tuple",LeftParen::tail->Result.map (fun (shapes,remaining)->TupleValue shapes,remaining) (parseArguments false tail)
 |"record",LeftParen::tail->Result.map (fun (shapes,remaining)->RecordValue shapes,remaining) (parseArguments false tail)
 |"sum",LeftParen::tail->let* shapes,remaining=parseArguments false tail in (match shapes with [payload]->Ok (SumValue payload,remaining)|_->Error "sum requires exactly one argument")
 |"closure",LeftParen::tail->Result.map (fun (shapes,remaining)->ClosureValue shapes,remaining) (parseArguments true tail)
 |name,LeftParen::_->Error ("Unknown managed shape '"^name^"'")|name,_->Error ("Unknown managed leaf shape '"^name^"'"))
 |_->Error "Expected a managed shape name" in
 let* tokens=tokenizeShape source in let* shape,remaining=parseOne tokens in if remaining=[] then Ok shape else Error "Unexpected tokens after ROOT managed shape"
let toSectionMap sections=
 match List.find_opt (fun (name,_)->not (StringOrder.Set.mem name knownSections)) sections with Some (name,_)->Error ("Unknown reference-release section: "^name)|None->
 let rec duplicate seen=function []->None|(name,_)::tail->if List.length (List.filter (fun (other,_)->other=name) sections)>1 && not (StringOrder.Set.mem name seen) then Some name else duplicate (StringOrder.Set.add name seen) tail in
 match duplicate StringOrder.Set.empty sections with Some name->Error ("Duplicate reference-release section: "^name)|None->Ok (M.of_list sections)
let required name sections=match M.find_opt name sections with Some value when Text.trim value<>""->Ok (Text.trim value)|Some _->Error ("Reference-release section "^name^" cannot be empty")|None->Error ("Missing required reference-release section: "^name)
let parsePreservedRegisters source=
 let parseLine (lineNumber,line)=match String.split_on_char '=' line |> List.map Text.trim with
 |[register;value]->let parsedReg=LIRParser.parsePhysReg register in let parsedVal=DSLPattern.int64 value in
   (match parsedReg,parsedVal with Ok register,Some value->Ok {register;value}|Error msg,_->Error (Printf.sprintf "PRESERVE line %d: %s" lineNumber msg)|_,_->Error (Printf.sprintf "PRESERVE line %d: invalid Int64 value '%s'" lineNumber value))
 |_->Error (Printf.sprintf "PRESERVE line %d: expected REGISTER = VALUE" lineNumber) in
 let lines=String.split_on_char '\n' source |> List.mapi (fun index line->index+1,Text.trim line) |> List.filter (fun (_,line)->line<>"") in
 let* reversed=List.fold_left (fun result line->let* parsed=result in Result.map (fun value->value::parsed) (parseLine line)) (Ok []) lines in Ok (List.rev reversed)
let parsePlacement sections=match M.find_opt "ROOT-REGISTER" sections,M.find_opt "PRESERVE" sections with
 |None,None->Ok CanonicalRoot|None,Some _->Error "PRESERVE requires ROOT-REGISTER"|Some register,preserved->
 let* rootRegister=LIRParser.parsePhysReg register in match preserved with None->Ok (ExplicitRoot (rootRegister,[]))|Some source->
 let* values=parsePreservedRegisters source in
 if List.exists (fun value->value.register=rootRegister) values then Error "PRESERVE register cannot also be ROOT-REGISTER"
 else if List.length (List.sort_uniq compare (List.map (fun value->value.register) values))<>List.length values then Error "PRESERVE registers must be unique" else Ok (ExplicitRoot (rootRegister,values))
let parseCase path sections=
 let* values=toSectionMap sections in let name=required "NAME" values in let root=required "ROOT" values in let placement=parsePlacement values in
 match name,root,placement with Error msg,_,_|_,Error msg,_|_,_,Error msg->Error msg|Ok name,Ok root,Ok placement->Result.map (fun root->{name;root;placement;sourceFile=path}) (parseShape root)
let groupCases sections=
 let rec loop completed current remaining=match remaining with
 |[]->Ok (List.rev (if current=[] then completed else List.rev current::completed))
 |("NAME",_ as section)::tail->if current=[] then loop completed [section] tail else loop (List.rev current::completed) [section] tail
 |section::tail->if current=[] then Error ("Reference-release case must start with NAME, found "^fst section) else loop completed (section::current) tail in loop [] [] sections
let parseRCReleaseFileContent path content=
 let sections=Common.parseSections (Common.normalizeLineEndings content) in if sections=[] then Error "Reference-release fixture contains no sections" else
 let* cases=groupCases sections in let* reversed=List.fold_left (fun result sections->let* parsed=result in Result.map (fun test->test::parsed) (parseCase path sections)) (Ok []) cases in Ok (List.rev reversed)
