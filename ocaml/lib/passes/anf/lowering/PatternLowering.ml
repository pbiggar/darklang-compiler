(* PatternLowering.fs - Lower ordered match alternatives and typed pattern projections. *)
[@@@warning "-4"]
module A = ANF
module C = CheckedAST
module R = TypeRegistries
module P = LoweringPrimitives
module K = Continuations
module M = StringOrder.Map
module Tags = Set.Make (Int)
let ( let* ) = Result.bind
let int value = A.IntLiteral (A.Int64 value)
let fmt = CheckedStructuralFormat.pattern
let showType = HostStructuralFormat.semanticType
let unknownTuple prefix patterns = List.mapi (fun index _ -> AST.TVar (prefix ^ string_of_int index)) patterns
let rec substituteTypeParams ?(records=true) subst typ =
 let sub = substituteTypeParams ~records subst in match typ with
 | AST.TVar name -> Option.value ~default:typ (M.find_opt name subst)
 | AST.TTuple fields -> AST.TTuple (List.map sub fields)
 | AST.TRecord (name, args) when records -> AST.TRecord (name, List.map sub args)
 | AST.TList elem -> AST.TList (sub elem)
 | AST.TDict (key, value) -> AST.TDict (sub key, sub value)
 | AST.TSum (name, args) -> AST.TSum (name, List.map sub args)
 | AST.TFunction (args, ret) -> AST.TFunction (List.map sub args, sub ret)
 | _ -> typ
let patternAlwaysMatches = function C.PUnit | C.PWildcard | C.PVariable _ -> true | _ -> false
let rec headNeedsStages = function
 | C.PString _ | C.PChar _ | C.PConstructor _ | C.PList _ | C.PListCons _ -> true
 | C.PTuple pats -> List.exists headNeedsStages pats
 | C.POr pats -> List.exists headNeedsStages (NonEmptyList.toList pats)
 | _ -> false
let listArmNeedsStages = function C.PList pats | C.PListCons (pats, _) -> List.exists headNeedsStages pats | _ -> false
let rec patternBindsVariablesEarly = function
 | C.PVariable _ -> true
 | C.PConstructor (_, pats) | C.PTuple pats | C.PList pats -> List.exists patternBindsVariablesEarly pats
 | C.PListCons (pats, tail) -> List.exists patternBindsVariablesEarly pats || patternBindsVariablesEarly tail
 | C.POr pats -> patternBindsVariablesEarly pats.NonEmptyList.head
 | _ -> false
let rec patternBindsVariables = function
 | C.PVariable _ -> true
 | C.PConstructor (_, pats) | C.PTuple pats | C.PList pats -> List.exists patternBindsVariables pats
 | C.PListCons (pats, tail) -> List.exists patternBindsVariables pats || patternBindsVariables tail
 | _ -> false
type collectionMode = General | TupleBody | Guard | Nested
(*
   Infer scrutinee type to pass to pattern extraction for correct typing
   Compile match to if-else chain
   First convert scrutinee to a bound atom. This supports effectful/complex
   scrutinees such as Builtin.testRuntimeError(...) that cannot be lowered via toAtom.
   Check if any pattern needs to access list structure
   If so, we must ensure scrutinee is a variable (can't TupleGet on literal)
   h :: t also needs list access
   If there are non-empty list patterns, bind the scrutinee to a variable
   Check if the TYPE that a variant belongs to has any variant with a payload
   This determines if values are heap-allocated or simple integers
   Check if pattern always matches (wildcard or variable)
   Constructor coverage is usable only when the payload pattern cannot
   reject a value; literal and nested patterns therefore remain partial.
   The lookup intentionally contains both qualified and bare
   aliases. Tags are the canonical per-type constructor identity,
   so collecting them also avoids counting those aliases twice.
   Extract pattern bindings and compile body with extended environment
   scrutType is the type of the scrutinee, used to determine correct types for pattern variables
   The list-pattern compilers take variables, wildcards, numeric literals and
   tuples of those as heads; a string, a constructor or a nested list there
   ("Unsupported head pattern in list cons") goes through the stages.
   Recursively collect all variable bindings from a pattern
   Returns: updated env, list of bindings, updated vargen
   sourceType is the type of the source being matched, used to get correct element types
   No variable bindings
   Bind the source to a variable with the correct type
   Use TypedAtom to preserve the semantic type (e.g., tuple element type)
   even when the source comes from a function with generic return type
   Extract each element and recursively collect bindings
   Extract raw element with TupleGet
   Wrap with TypedAtom to preserve correct element type in TypeMap
   Recursively collect bindings from this element's pattern with correct type
   Extract element type from list type
   For list patterns, extract head elements using SkewList operations
   Use _i64 versions which work for any element type at runtime (all values are 64-bit)
   The correct element type is tracked in the VarEnv/TypeMap, not in the function name
   Lists are SkewLists - use headUnsafe/tail to extract
   Get tail for next iteration
   Extract head elements then bind tail using SkewList operations
   Bind the remaining list to tail pattern (tail has same type as source)
   Wrap tail with TypedAtom to preserve list type
   Heads that are strings, constructors or lists: the specialized
   list compilers below do not take them, the general collector does.
   Bind scrutinee to variable name with the correct type
   Project the payload according to this sum's representation.
   Apply type substitution if scrutType has type args
   Project the payload and recursively collect bindings.
   Collect all bindings from the tuple pattern, then compile body
   Extract list elements from SkewList structure
   SkewList layout:
   SINGLE (tag 1): [node:8] where node is LEAF-tagged
   DEEP (tag 2): [measure:8][prefixCount:8][p0:8][p1:8][p2:8][p3:8][middle:8][suffixCount:8][s0:8][s1:8][s2:8][s3:8]
   LEAF (tag 5): [value:8]
   Get element type from list type
   Helper to unwrap a LEAF node and get the value
   Helper to extract tuple elements from a value
   tupleType is the type of the tuple being destructured
   Empty pattern - no bindings needed
   SINGLE node: extract the single element
   Untag to get pointer to SINGLE structure
   Get the complete-tree root from the digit.
   Leaf and internal tree nodes both store their value at offset 0.
   Wrap with TypedAtom to preserve element type in TypeMap
   Bind the pattern
   elemType is the list element type, use it as tuple type
   Traverse exact-list patterns through the representation API.
   Extract head elements and bind tail using SkewList operations
   Lists are SkewLists, use headUnsafe_i64/tail_i64 for extraction
   Extract head using SkewList.headUnsafe_i64
   Extract tail using SkewList.tail_i64
   Wrap with TypedAtom to preserve list type for tail
   All bindings including raw extractions
   Order: typedBindings first (will be reversed at line 3923), so after reversal raw bindings come before typed
   For tuple patterns inside list cons, extract each tuple element and bind variables
   elemType is the tuple type (since list elements are tuples)
   Wrap with TypedAtom to preserve correct element type
   Bind tail pattern
   Tail has the same list type as the scrutinee
   Extract pattern bindings, check guard, and compile body
   Returns: if guard is true, execute body; otherwise execute elseExpr
   scrutType is the type of the scrutinee for correct pattern variable typing
   First, we need to extract bindings from the pattern
   Then compile the guard with those bindings in scope
   Then compile the body with those bindings in scope
   Finally, generate: let <bindings> in if <guard> then <body> else <elseExpr>
   Return true only when we can prove a pattern can never match this type.
   Used to preserve "fall through" semantics for guarded patterns that should not bind.
   Helper to collect pattern variable bindings (simplified version for common patterns)
   sourceType is the type of the source being matched
   Use TypedAtom to preserve the correct type in TypeMap
   Compile guard expression in the extended environment
   Compile body expression in the extended environment
   Build: if guard then body else elseExpr
   Wrap guard bindings
   Wrap pattern bindings (in reverse order since we accumulated in reverse)
   Build comparison expression for a pattern
   A constructor pattern's variant is looked up in the type of the value it
   tests, which for a nested pattern is the enclosing variant's payload, not
   the match scrutinee. Looked up by bare name, `| Some(String "2.0")` on an
   Option<Json> found whatever type registered a `String` variant last, with
   that type's tag, and then compared the payload as a string: wrong arm on
   a Number, SIGSEGV on a Float payload.
   `patType` is the static type of the value `scrutAtom` holds, when known;
   None falls back to the match scrutinee's type.
   Unit pattern always matches unit type
   String patterns must use byte-wise equality, not pointer equality.
   Char values are represented as single-EGC strings at runtime.
   Distinguish -0.0 from 0.0 using reciprocal sign.
   The fields' types, with the tested type's arguments substituted.
   Constructor arity mismatch in pattern should not match.
   Mixed or payload-carrying sum type: tag is stored in heap at index 0.
   Extract payload and check inner pattern if needed.
   Inner pattern is variable/wildcard, only need tag check.
   Nullary variant in a payload-mixed sum type: only check tag.
   Simple enum (no payload variants in the type): scrutinee IS the tag.
   Tuple patterns with literals need to compare each element
   All variables/wildcards, no comparison needed
   AND together all conditions
   Extract element at index
   Check if this pattern needs comparison
   This element pattern doesn't need comparison (var/wildcard)
   Add this comparison
   Exact list patterns compare the cached skew-list length.
   Empty list pattern: check scrutinee == 0
   Multiple elements: check length == patternLen
   Use Stdlib.List.__length which handles EMPTY/SINGLE/DEEP safely
   A cons pattern needs one element per normalized head before binding its tail;
   a :: b :: t needs at least two, etc.
   Collect variable bindings for nested patterns under a value that is already known to match.
   This is used by list/list-cons lowering where structural checks are emitted separately.
   Compile a list pattern for SkewList with proper length validation.
   listType is the list type (TList elemType) for correct pattern variable typing
   List patterns on non-list scrutinees are definite non-matches.
   This can happen after grouped-pattern desugaring where non-first alternatives
   were not type-checked against the scrutinee shape.
   tupleType is the type of the tuple being matched (TTuple elemTypes)
   Use correct element type
   Empty list: check scrutinee == 0 (EMPTY)
   A singleton has one digit whose tree pointer is at offset 16.
   Literal pattern: check tag==SINGLE, extract value, check value==literal
   Important: bindings must come BEFORE the literal check since they define valueVar
   Structure: check tag -> extract value (bindings) -> check literal -> if match then body else else
   Note: We use two nested Ifs because the tag check guards the memory access in bindings
   bindings must be OUTSIDE the inner If to define valueVar before litCheckExpr uses it
   Use element type
   Structural comparison must run before extracting binders so a
   failed nested pattern cannot leak its bindings into this arm.
   Multiple elements: check length == patternLen (safe for all list types)
   Untag to get pointer (only used in then-branch after length check passes)
   Note: lengthExpr and checkExpr are safe (length handles EMPTY)
   ptrExpr just does bitwise and, doesn't dereference
   Keep consistent naming for the rest of the code
   Extract elements using getAt (handles varying prefix/suffix layouts)
   Returns: (env, bindings, conditionAtoms, vg)
   Use getAt to retrieve element at this index
   getAt returns Option, but we know length == patternLen so it's always Some
   Select a type-specific wrapper to avoid defaulting to Int64 for floats.
   Monomorphization happens at AST level, so we must use a non-generic wrapper here.
   Unwrap the Some - getAt returns tagged value with tag 1 for Some
   Some payload at offset 8
   Build the inner expression based on whether we have extra conditions
   No extra conditions - just return body
   AND condition atoms together (length check is handled separately by checkVar)
   Wrap with element bindings (inside length check)
   Wrap with length check
   Compile a list cons pattern [h, ...t] for SkewList
   This pattern extracts head element(s) and binds the rest to tail
   For SkewList:
   - SINGLE (tag 1): head is the element, tail is EMPTY
   - DEEP (tag 2): head is prefix[0], tail requires calling SkewList.tail
   Helper to extract tuple elements
   Extract element types from tuple type
   Even for wildcard, we need to extract the element (for proper tuple access)
   but don't bind it to a name. Just add the raw binding and continue.
   All head elements extracted - bind tail and compile body
   Use actual list type
   Single head pattern [h, ...t] - most common case
   Use branching based on tag to handle SINGLE vs DEEP nodes
   Check list is not empty
   Get tag
   Untag to get pointer
   The old single/deep distinction no longer exists. Route every
   non-empty digit through the generic tree-root branch below.
   notEmptyVar must be bound OUTSIDE the If since it's used as the condition
   Compile the SINGLE branch: node at offset 0, tail = EMPTY
   Wrap headVar with TypedAtom to preserve correct element type in TypeMap
   Tail is empty list (0 = EMPTY sentinel) - wrap with TypedAtom to preserve list type
   EMPTY
   Bind head pattern - returns (env, tupleBindings, vg, guardOpt)
   guardOpt is Some(var, expr) for literal patterns that need comparison
   Use typed head var with element type
   Compare head value to literal - guard check
   If there's a guard (literal pattern), add check AFTER head bindings
   because guardExpr uses headVar which is defined in headBindings
   headBindingsWithType -> guardVar -> if guard then body else elseExpr
   Compile the DEEP branch: node at offset 16 (prefix[0])
   For tail, call Stdlib.List.__tail to properly compute the tail
   Call Stdlib.List.__tail to get the tail
   Wrap with TypedAtom to preserve correct list type in TypeMap
   Use typed tail var with correct list type
   because guardExpr uses headVar which is defined in headBindingsWithType
   Build the combined expression with branching
   If SINGLE then singleBranch else deepBranch
   Wrap inner bindings (tag, ptr, isSingle) around the tag branch
   If not empty then execute inner bindings + branch else elseExpr
   Bind notEmptyVar BEFORE the If
   Multiple head patterns [a, b, ...t]
   Check length >= number of head patterns
   Extract head elements and final tail using head/tail calls
   No more head patterns, currentListVar is the tail
   Call head to get current element
   Call tail to get rest
   Preserve type information for both head and tail values.
   Get initial list variable
   Compile the tail after extracting the chained heads. Exact-list and
   further-cons tails need their full checked lowering: a comparison-only
   PList condition validates length but does not compare its elements.
   Build pattern condition checks first; these checks may depend on
   vars extracted by headBindings/tailBindings, so extraction wraps outside.
   Apply tail/head extraction bindings (including comparison inputs) outside guard checks.
   Check length condition first
   Build OR of multiple pattern conditions for pattern grouping
   Returns: combined condition atom, all bindings, updated vargen
   The tests a pattern makes on a value, in the order they must run. A
   stage's bindings are only safe once every earlier stage's condition
   held: a variant's payload slot is a payload only when its tag matched,
   and past a smaller variant it is whatever the heap holds there. The
   flat comparison above runs every load and test at once, so a nested
   pattern that dereferences the payload (a constructor's tag load, a
   string compare) reads through garbage: `| Some((_, String "2.0"))` on a
   None or on a Some(Number) was a SIGSEGV. An arm compiled from stages
   nests one `If` per stage instead. The else branch is repeated per
   stage, which is what the list-pattern compilers already do.
   Nullary, enum-only or unknown: the flat comparison is one stage.
   The empty skew-list is the zero tagged pointer; avoid a
   full length traversal for the hottest list base case.
   A list pattern below the top of an arm (in a tuple, a payload) was
   one flat length test, so `("Stdlib" :: _, x)` matched every
   non-empty list. The length is one stage, then each head is taken
   and tested in turn, then the tail.
   `thenExpr` under every stage's condition, `elseExpr` when any fails,
   each used once. One stage is a plain `If`. Several become a Bool join:
   the entry runs the stages in order, jumping out with `false` at the
   first failed test and with the last condition otherwise, and the
   continuation is the `If` on that Bool. Nesting `If`s instead would
   repeat `elseExpr` per stage, and it is the rest of the match: a match
   with n such arms would copy its tail 2^n times.
   Build comparison for each pattern, then OR them together
   Pattern always matches (wildcard/var) - the whole group always matches
   First condition
   OR with previous conditions
   Put bindings in dependency order: comparison bindings first, OR at end
   (foldBack makes first binding outermost, so dependencies must come first)
   Build the if-else chain from cases
   No cases left - shouldn't happen if we have wildcard/var
   Desugar grouped patterns (`p1 | p2 -> body`) into sequential single-pattern
   cases so bindings come from the pattern that actually matched.
   The specialized list/list-cons compilers already emit complete
   matching checks plus fallback, so a separate pre-comparison
   condition here would duplicate work and code size.
   Never taken, but the arm is still compiled: an unknown
   constructor or an ill-typed body must still be an error.
   Pattern always matches; guard (if any) decides between body/fallback.
   For pattern grouping, use first pattern for bindings but OR all patterns for comparison
   Wildcard or var - matches everything, but may still need guard
   Wildcard with guard - still need to check guard, fall through if false
   Non-empty list patterns need special handling with interleaved
   checks. The list compilers take the body as is, so an arm with a
   `when` guard goes through the stages below, which test the guard
   after the pattern and fall through to the rest when it is false.
   Build the else branch first (rest of cases)
   Use the new interleaved check-and-extract function
   List cons pattern - needs interleaved checks
   Use pattern grouping: OR all patterns in the group
   Pattern always matches
   Pattern match + guard: if pattern matches, bind, check guard
*)
let lowerMatch (toANFCore : LoweringCallbacks.expressionLowerer)
 (toAtomCore : LoweringCallbacks.atomLowerer)
 (toANFBoundAtomCore : LoweringCallbacks.boundAtomLowerer)
 functionIds sums typeNames inert scrutinee cases gen env registry variants functions names modules =
 let constructorTag id = match R.tryFindConstructorTag id typeNames with Some tag -> tag | None -> Crash.crash "Checked constructor identity is absent from layout metadata" in
 let functionId name = match M.find_opt name functionIds with Some id -> id | None -> Crash.crash ("Pattern lowering function '" ^ name ^ "' is absent from registries") in
 let anf value gen env = toANFCore sums typeNames inert value gen env registry variants functions names modules in
 let atom value gen env = toAtomCore sums typeNames inert value gen env registry variants functions names modules in
 let* scrutType = Result.map_error (fun message -> "Match scrutinee type inference failed: " ^ message)
   (LoweringTypeInference.inferTypeCore sums typeNames scrutinee (R.typeEnvFromVarEnv env) registry variants functions names modules) in
 let* scrutineeExpr, scrutineeAtom, gen = toANFBoundAtomCore sums typeNames inert scrutinee gen env registry variants functions names modules in
 let hasNonEmptyListPattern = List.exists (fun case -> List.exists (function C.PList (_ :: _) | C.PListCons (_ :: _, _) -> true | _ -> false) (NonEmptyList.toList case.C.patterns)) cases in
 let scrutineeAtom, postBindings, gen = match scrutineeAtom with
 | A.Var _ -> scrutineeAtom, [], gen
 | _ when hasNonEmptyListPattern -> let id, gen = A.freshVar gen in A.Var id, [id, A.Atom scrutineeAtom], gen
 | _ -> scrutineeAtom, [], gen in
 let variant id typ = P.tryFindVariantForTypeById id typ typeNames variants in
 let hasPayload name = M.exists (fun _ (owner, _, _, fields) -> owner = name && fields <> []) variants in
 let unboxed = function AST.TSum (name, args) -> Option.is_some (P.nullablePointerSumPayloadType name args sums.P.cases) || Option.is_some (P.spareImmediateSumSentinel name args sums.P.cases) | _ -> false in
 let transparent = function AST.TSum (name, args) -> P.transparentSumPayloadType name args sums.P.cases | _ -> None in
 let absentWord = function AST.TSum (name, args) -> Option.value ~default:0L (P.spareImmediateSumSentinel name args sums.P.cases) | _ -> 0L in
 let spare = function AST.TSum (name, args) -> Option.is_some (P.spareImmediateSumSentinel name args sums.P.cases) | _ -> false in
 let resolveFields ?(records=true) ?(canonical=true) id typ =
  match variant id typ with None -> Error ("Unknown constructor tag '" ^ string_of_int (constructorTag id) ^ "' in pattern")
  | Some (_, params, _, fields) ->
    let fields = match typ with AST.TSum (_, args) when List.length params = List.length args ->
      List.map (substituteTypeParams ~records (M.of_seq (List.to_seq (List.combine params args)))) fields | _ -> fields in
    Ok (if canonical then List.map (R.canonicalizeBareSumTypeRefs variants) fields else fields) in
 let payload pats fields = match pats, fields with [pat], [typ] -> pat, typ | _ -> C.PTuple pats, AST.TTuple fields in
 let head typ value = R.listHeadUnsafeExpr functionIds typ value in
 let tail value = A.Call (functionId "Darklang.Stdlib.List.__tail_i64", [value]) in
 let makeFalsePatternCondition gen = let id, gen = A.freshVar gen in A.Var id, [id, A.Atom (A.BoolLiteral false)], gen in
 let rec collect mode pat source typ env bindings gen =
  let prepend = mode <> Nested in
  let append extra = if prepend then List.rev extra @ bindings else bindings @ extra in
  match pat with
  | _ when (mode = General || mode = TupleBody) && not (patternBindsVariablesEarly pat) -> Ok (env, bindings, gen)
  | C.POr pats -> collect mode pats.NonEmptyList.head source typ env bindings gen
  | C.PVariable name -> let id, gen = A.freshVar gen in Ok (R.BindingMap.add name (id, typ) env, append [id, A.TypedAtom (source, typ)], gen)
  | C.PTuple pats ->
    let types = match typ with
    | AST.TTuple types when List.length types = List.length pats -> Some types
    | AST.TTuple types when mode = Guard -> Crash.crash (Printf.sprintf "collectBindings(PTuple): expected %d tuple elements, got %d" (List.length pats) (List.length types))
    | _ when mode = Guard -> Crash.crash ("collectBindings(PTuple): expected tuple source type, got " ^ P.typeToString typ)
    | AST.TVar name when mode = Nested -> Some (unknownTuple ("__tuple_elem_" ^ name ^ "_") pats)
    | AST.TNever when mode = Nested -> Some (unknownTuple "__tuple_elem_runtime_error_" pats)
    | _ when mode = Nested -> None
    | _ -> Some (unknownTuple "__tuple_elem_" pats) in
    (match types with None -> Ok (env, bindings, gen) | Some types ->
    let rec loop pats types index env bindings gen = match pats, types with
    | [], _ -> Ok (env, bindings, gen)
    | pat :: rest, typ :: types ->
      let raw, gen = A.freshVar gen in
      let value, extra, gen = if mode = General || mode = TupleBody then
        let typed, gen = A.freshVar gen in A.Var typed, [raw, A.TupleGet (source, index); typed, A.TypedAtom (A.Var raw, typ)], gen
        else A.Var raw, [raw, A.TupleGet (source, index)], gen in
      let bindings = if prepend then List.rev extra @ bindings else bindings @ extra in
      let* env, bindings, gen = collect mode pat value typ env bindings gen in loop rest types (index + 1) env bindings gen
    | _ when mode = Guard -> Crash.crash (Printf.sprintf "collectBindings(PTuple): missing tuple element type at index %d; %d pattern elements remain" index (List.length pats))
    | _ -> Error "Tuple pattern element/type mismatch" in loop pats types 0 env bindings gen)
  | C.PConstructor (id, pats) -> (match pats with [] -> Ok (env, bindings, gen) | _ ->
    let* fields = resolveFields ~canonical:(mode <> TupleBody) id typ in
    if mode = General && List.length pats <> List.length fields then Ok (env, bindings, gen) else
    let pat, ptype = payload pats fields in let id, gen = A.freshVar gen in
    collect mode pat (A.Var id) ptype env (append [id, P.sumPayloadExpr typ source sums.P.cases]) gen)
  | C.PList pats | C.PListCons (pats, _) ->
    let isCons = match pat with C.PListCons _ -> true | _ -> false in
    let* elem = match typ with
    | AST.TList elem -> Ok elem
    | AST.TVar name when mode = Nested -> Ok (AST.TVar ("__list_elem_" ^ name))
    | AST.TNever when mode = Nested -> Ok (AST.TVar "__list_elem_runtime_error")
    | AST.TVar _ | AST.TNever -> Ok (AST.TVar "__list_elem_unknown")
    | _ when mode = General || mode = TupleBody -> Ok (AST.TVar "__list_elem_unknown")
    | _ -> let label = if isCons then "PListCons" else "PList" in
      Error (if mode = Guard then "collectBindings(" ^ label ^ "): expected list-compatible source type, got " ^ P.typeToString typ
      else label ^ " nested binding expects list source type, got " ^ P.typeToString typ) in
    let rec loop pats current env bindings gen = match pats with
    | [] -> (match pat with C.PListCons (_, tailPat) -> collect mode tailPat current typ env bindings gen | _ -> Ok (env, bindings, gen))
    | item :: rest ->
      let rawHead, gen = A.freshVar gen in
      let value, extra, gen = if isCons && (mode = General || mode = TupleBody) then
        let typed, gen = A.freshVar gen in A.Var typed, [rawHead, head elem current; typed, A.TypedAtom (A.Var rawHead, elem)], gen
        else A.Var rawHead, [rawHead, head elem current], gen in
      if isCons && mode = Nested then
        let tailId, gen = A.freshVar gen in
        let* env, bindings, gen = collect mode item value elem env (bindings @ extra @ [tailId, tail current]) gen in
        loop rest (A.Var tailId) env bindings gen
      else
        let bindings = if prepend then List.rev extra @ bindings else bindings @ extra in
        let* env, bindings, gen = collect mode item value elem env bindings gen in
        if not isCons && rest = [] then Ok (env, bindings, gen) else
        let rawTail, gen = A.freshVar gen in
        let value, extra, gen = if isCons && (mode = General || mode = TupleBody) then
          let typed, gen = A.freshVar gen in A.Var typed, [rawTail, tail current; typed, A.TypedAtom (A.Var rawTail, typ)], gen
          else A.Var rawTail, [rawTail, tail current], gen in
        let bindings = if prepend then List.rev extra @ bindings else bindings @ extra in
        loop rest value env bindings gen in loop pats source env bindings gen
  | C.PInt64 _ | C.PBigInt _ | C.PInt128Literal _ | C.PInt8Literal _ | C.PInt16Literal _ | C.PInt32Literal _
  | C.PUInt8Literal _ | C.PUInt16Literal _ | C.PUInt32Literal _ | C.PUInt64Literal _ | C.PUInt128Literal _
  | C.PUnit | C.PWildcard | C.PBool _ | C.PString _ | C.PChar _ | C.PFloat _ -> Ok (env, bindings, gen)
 in
 let rec patternDefinitelyCannotMatchType pat typ = match pat, typ with
 | C.PTuple pats, AST.TTuple types -> List.length pats <> List.length types || List.exists2 patternDefinitelyCannotMatchType pats types
 | C.PTuple _, AST.TVar _ -> false
 | C.PTuple _, _ -> true
 | C.PList pats, AST.TList elem -> List.exists (fun pat -> patternDefinitelyCannotMatchType pat elem) pats
 | C.PList _, AST.TVar _ -> false
 | C.PList _, _ -> true
 | C.PListCons (pats, tail), AST.TList elem -> List.exists (fun pat -> patternDefinitelyCannotMatchType pat elem) pats || patternDefinitelyCannotMatchType tail typ
 | C.PListCons _, AST.TVar _ -> false
 | C.PListCons _, _ -> true
 | C.POr pats, _ -> List.for_all (fun pat -> patternDefinitelyCannotMatchType pat typ) (NonEmptyList.toList pats)
 | _ -> false in
 let rec patternStaticallyCannotMatchType pat typ = match pat, typ with
 | C.PTuple pats, AST.TTuple types -> List.length pats <> List.length types || List.exists2 patternStaticallyCannotMatchType pats types
 | (C.PTuple _ | C.PList _ | C.PListCons _ | C.PConstructor _), (AST.TVar _ | AST.TNever) -> false
 | C.PConstructor (id, pats), (AST.TSum (_, args) | AST.TRecord (_, args)) ->
   (match variant id typ with None -> true | Some (_, params, _, fields) ->
    List.length pats <> List.length fields ||
    let subst = if List.length params = List.length args then M.of_seq (List.to_seq (List.combine params args)) else M.empty in
    List.exists2 (fun pat typ -> patternStaticallyCannotMatchType pat (R.canonicalizeBareSumTypeRefs variants (TypeSubstitution.applySubstToType subst typ))) pats fields)
 | C.PList pats, AST.TList elem -> List.exists (fun pat -> patternStaticallyCannotMatchType pat elem) pats
 | C.PListCons (pats, tail), AST.TList elem -> List.exists (fun pat -> patternStaticallyCannotMatchType pat elem) pats || patternStaticallyCannotMatchType tail typ
 | (C.PTuple _ | C.PList _ | C.PListCons _ | C.PConstructor _), _ -> true
 | _ -> false in
 let _substituteTypeForStaticPatternCheck = substituteTypeParams in
 let literalComparison pat value = match pat with
 | C.PInt128Literal n -> P.int128LiteralComparison functionId value n
 | C.PUInt128Literal n -> P.uint128LiteralComparison functionId value n
 | _ -> (match P.patternLiteralToSizedInt pat with Some n -> A.Prim (A.Eq, value, A.IntLiteral n) | None -> Crash.crash ("Expected integer literal pattern, got " ^ fmt pat)) in
 let numeric = function C.PInt64 _ | C.PInt8Literal _ | C.PInt16Literal _ | C.PInt32Literal _ | C.PUInt8Literal _ | C.PUInt16Literal _ | C.PUInt32Literal _ | C.PUInt64Literal _ | C.PInt128Literal _ | C.PUInt128Literal _ -> true | _ -> false in
 let numericIncludingBig pat = numeric pat || (match pat with C.PBigInt _ -> true | _ -> false) in
 let unwrapLeaf elem leaf gen bindings = let ptr, gen = A.freshVar gen in let value, gen = A.freshVar gen in
   A.Var value, value, bindings @ [ptr, A.Prim (A.BitAnd, leaf, int (-8L)); value, A.RawGet (A.Var ptr, int 0L, if elem = AST.TFloat64 then Some AST.TFloat64 else None)], gen in
 let singletonLoads elem source gen =
  let ptr, gen = A.freshVar gen in let node, gen = A.freshVar gen in
  let value, _, bindings, gen = unwrapLeaf elem (A.Var node) gen [ptr, A.Prim (A.BitAnd, source, int (-8L)); node, A.RawGet (A.Var ptr, int 16L, None)] in
  let typed, gen = A.freshVar gen in A.Var typed, typed, bindings @ [typed, A.TypedAtom (value, elem)], gen in
 let tupleVariables pats source typ env bindings gen =
  let rec loop pats index env bindings gen =
   let* types = match typ with AST.TTuple types when List.length types >= List.length pats -> Ok types
    | AST.TTuple types -> Error (Printf.sprintf "Tuple pattern expects %d elements but got %d" (List.length pats) (List.length types))
    | _ -> Error ("Tuple pattern expects tuple elements, got " ^ P.typeToString typ) in
   match pats with [] -> Ok (env, bindings, gen) | pat :: rest ->
    let id, gen = A.freshVar gen in let binding = id, A.TupleGet (source, index) in let typ = List.nth types index in
    (match pat with C.PVariable name -> loop rest (index + 1) (R.BindingMap.add name (id, typ) env) (bindings @ [binding]) gen
    | C.PWildcard -> loop rest (index + 1) env bindings gen
    | _ -> Error ("Nested pattern in tuple element not yet supported: " ^ fmt pat)) in loop pats 0 env bindings gen in
 let rec extractAndCompileBody pat body source typ env gen =
  match pat with
  | (C.PList _ | C.PListCons _) when listArmNeedsStages pat ->
    let* env, bindings, gen = collect General pat source typ env [] gen in
    let* body, gen = anf body gen env in Ok (K.wrapBindings (List.rev bindings) body, gen)
  | C.POr pats -> extractAndCompileBody pats.NonEmptyList.head body source typ env gen
  | C.PVariable name -> let id, gen = A.freshVar gen in
    let* body, gen = anf body gen (R.BindingMap.add name (id, typ) env) in Ok (A.Let (id, A.Atom source, body), gen)
  | C.PConstructor (id, pats) -> (match pats with [] -> anf body gen env | _ ->
    match variant id typ with None -> Error ("Constructor tag '" ^ string_of_int (constructorTag id) ^ "' not found in variant lookup for scrutinee type '" ^ showType typ ^ "' and constructor '" ^ HostStructuralFormat.format (AST.DiagnosticFormatting.constructor id) ^ "'")
    | Some _ ->
      let raw, gen = A.freshVar gen in let typed, gen = A.freshVar gen in
      let* fields = resolveFields ~records:false id typ in let inner, ptype = payload pats fields in
      let* body, gen = extractAndCompileBody inner body (A.Var typed) ptype env gen in
      Ok (K.wrapBindings [raw, P.sumPayloadExpr typ source sums.P.cases; typed, A.TypedAtom (A.Var raw, ptype)] body, gen))
  | C.PTuple _ -> let* env, bindings, gen = collect TupleBody pat source typ env [] gen in
    let* body, gen = anf body gen env in Ok (K.wrapBindings (List.rev bindings) body, gen)
  | C.PList pats ->
    let elem = match typ with AST.TList elem -> elem | AST.TVar name -> AST.TVar ("__list_elem_" ^ name) | AST.TNever -> AST.TVar "__list_elem_runtime_error" | _ -> AST.TVar "__list_elem_unknown" in
    let bind pat value id env bindings gen = match pat with
     | C.PVariable name -> Ok (R.BindingMap.add name (id, elem) env, bindings, gen)
     | C.PWildcard -> Ok (env, bindings, gen)
     | _ when numericIncludingBig pat -> Ok (env, bindings, gen)
     | C.PTuple pats -> tupleVariables pats value elem env bindings gen
     | C.PConstructor _ | C.PList _ | C.PListCons _ when List.length pats = 1 -> Error "Nested pattern in list element not yet supported"
     | _ -> Error ((if List.length pats = 1 then "Unsupported pattern in single-element list: " else "Unsupported pattern in list element: ") ^ fmt pat) in
    let* env, bindings, gen = match pats with [] -> Ok (env, [], gen)
    | [pat] -> let value, id, bindings, gen = singletonLoads elem source gen in bind pat value id env bindings gen
    | _ -> let rec loop pats current env bindings gen = match pats with [] -> Ok (env, bindings, gen)
      | pat :: rest -> let raw, gen = A.freshVar gen in let typed, gen = A.freshVar gen in
        let rawTail, gen = A.freshVar gen in let typedTail, gen = A.freshVar gen in
        let bindings = bindings @ [raw, head elem current; typed, A.TypedAtom (A.Var raw, elem); rawTail, tail current; typedTail, A.TypedAtom (A.Var rawTail, AST.TList elem)] in
        let* env, bindings, gen = bind pat (A.Var typed) typed env bindings gen in loop rest (A.Var typedTail) env bindings gen
      in loop pats source env [] gen in
    let* body, gen = anf body gen env in Ok (K.wrapBindings bindings body, gen)
  | C.PListCons (pats, tailPat) ->
    let elem = match typ with AST.TList elem -> elem | _ -> Crash.crash ("PListCons pattern expects TList scrutinee in extractAndCompileBody, got " ^ showType typ) in
    let tupleBindings pats source elem env bindings gen =
     let types = match elem with AST.TTuple types -> types | AST.TVar name -> unknownTuple ("__tuple_elem_" ^ name ^ "_") pats | AST.TNever -> unknownTuple "__tuple_elem_runtime_error_" pats | _ -> unknownTuple "__tuple_elem_unknown_" pats in
     let rec loop pats types index env bindings gen = match pats with [] -> Ok (env, bindings, gen) | pat :: rest ->
      let typ = match types with typ :: _ -> typ | [] -> AST.TVar ("__tuple_elem_unknown_" ^ string_of_int index) in
      let restTypes = match types with _ :: rest -> rest | [] -> [] in
      let raw, genRaw = A.freshVar gen in let typed, genTyped = A.freshVar genRaw in
      (match pat with C.PVariable name -> loop rest restTypes (index + 1) (R.BindingMap.add name (typed, typ) env) (bindings @ [raw, A.TupleGet (source, index); typed, A.TypedAtom (A.Var raw, typ)]) genTyped
      | C.PWildcard -> loop rest restTypes (index + 1) env (bindings @ [raw, A.TupleGet (source, index)]) genRaw
      | _ -> Error ("Nested pattern in tuple element not yet supported: " ^ fmt pat)) in loop pats types 0 env bindings gen in
    let rec loop pats current env bindings gen = match pats with [] -> Ok (env, bindings, current, gen) | pat :: rest ->
      let raw, gen = A.freshVar gen in let typed, gen = A.freshVar gen in let rawTail, gen = A.freshVar gen in let typedTail, gen = A.freshVar gen in
      let bindings = bindings @ [raw, head elem current; typed, A.TypedAtom (A.Var raw, elem); rawTail, tail current; typedTail, A.TypedAtom (A.Var rawTail, typ)] in
      let* env, bindings, gen = match pat with C.PVariable name -> Ok (R.BindingMap.add name (typed, elem) env, bindings, gen)
       | C.PWildcard -> Ok (env, bindings, gen) | _ when numericIncludingBig pat -> Ok (env, bindings, gen)
       | C.PTuple pats -> tupleBindings pats (A.Var typed) elem env bindings gen
       | _ -> Error ("Nested pattern in list cons element not yet supported: " ^ fmt pat) in loop rest (A.Var typedTail) env bindings gen in
    let* env, bindings, remaining, gen = loop pats source env [] gen in
    (match tailPat with C.PVariable name -> let id, gen = A.freshVar gen in
      let* body, gen = anf body gen (R.BindingMap.add name (id, typ) env) in Ok (K.wrapBindings (bindings @ [id, A.TypedAtom (remaining, typ)]) body, gen)
    | C.PWildcard -> let* body, gen = anf body gen env in Ok (K.wrapBindings bindings body, gen)
    | _ -> Error "Tail pattern in list cons must be variable or wildcard")
  | C.PUnit | C.PWildcard | C.PInt64 _ | C.PBigInt _ | C.PInt128Literal _ | C.PInt8Literal _ | C.PInt16Literal _ | C.PInt32Literal _
  | C.PUInt8Literal _ | C.PUInt16Literal _ | C.PUInt32Literal _ | C.PUInt64Literal _ | C.PUInt128Literal _ | C.PBool _ | C.PString _ | C.PChar _ | C.PFloat _ -> anf body gen env
 in
 let extractAndCompileBodyWithGuard pat guard body source typ env gen elseExpr =
  if patternDefinitelyCannotMatchType pat typ then Ok (elseExpr, gen) else
  let* env, bindings, gen = collect Guard pat source typ env [] gen in
  let* condition, guardBindings, gen = atom guard gen env in
  let* body, gen = anf body gen env in
  Ok (K.wrapBindings (List.rev bindings) (K.wrapBindings guardBindings (A.If (condition, body, elseExpr))), gen) in
 let rec buildPatternComparison pat source patType gen =
  let typ = Option.value ~default:scrutType patType in
  let single expression gen = let id, gen = A.freshVar gen in Ok (Some (A.Var id, [id, expression], gen)) in
  match pat with
  | C.POr pats -> buildPatternComparison pats.NonEmptyList.head source patType gen
  | C.PUnit | C.PWildcard | C.PVariable _ -> Ok None
  | _ when numeric pat -> single (literalComparison pat source) gen
  | C.PBigInt value ->
    let literal, gen = A.freshVar gen in
    let expression = if Z.compare value (Z.neg (Z.shift_left Z.one 62)) >= 0 && Z.compare value (Z.pred (Z.shift_left Z.one 62)) <= 0 then
      A.TypedAtom (int (Z.to_int64 (Z.succ (Z.mul value (Z.of_int 2)))), AST.TInt)
      else A.Call (functionId "Darklang.Stdlib.Int.__value", [A.StringLiteral (Z.to_string value)]) in
    let compare, gen = A.freshVar gen in
    Ok (Some (A.Var compare, [literal, expression; compare, A.Call (functionId "Darklang.Stdlib.Int.__equals", [source; A.Var literal])], gen))
  | C.PBool value -> single (A.Prim (A.Eq, source, A.BoolLiteral value)) gen
  | C.PString value -> single (A.CanonicalBufferEq (MemoryModel.Utf8String, source, A.StringLiteral (HostText.normalize value))) gen
  | C.PChar value -> single (A.CanonicalBufferEq (MemoryModel.GraphemeCluster, source, A.StringLiteral (HostText.normalize value))) gen
  | C.PFloat value when value = 0. ->
    let zero, gen = A.freshVar gen in let reciprocal, gen = A.freshVar gen in let sign, gen = A.freshVar gen in let combined, gen = A.freshVar gen in
    let target = if Int64.bits_of_float value < 0L then neg_infinity else infinity in
    Ok (Some (A.Var combined, [zero, A.Prim (A.Eq, source, A.FloatLiteral 0.); reciprocal, A.Prim (A.Div, A.FloatLiteral 1., source); sign, A.Prim (A.Eq, A.Var reciprocal, A.FloatLiteral target); combined, A.Prim (A.And, A.Var zero, A.Var sign)], gen))
  | C.PFloat value -> single (A.Prim (A.Eq, source, A.FloatLiteral value)) gen
  | C.PConstructor (id, pats) -> (match variant id typ with
    | None -> Error ("Unknown constructor tag in pattern: " ^ string_of_int (constructorTag id))
    | Some (name, params, tag, fields) ->
      let fields = match typ with AST.TSum (_, args) when List.length params = List.length args -> List.map (substituteTypeParams (M.of_seq (List.to_seq (List.combine params args)))) fields | _ -> fields in
      let ptype = match fields with [] -> None | [typ] -> Some typ | _ -> Some (AST.TTuple fields) in
      if List.length pats <> List.length fields then single (A.Atom (A.BoolLiteral false)) gen
      else if unboxed typ then
        let cmp, vg1 = A.freshVar gen in let expression = A.Prim ((if pats = [] then A.Eq else A.Neq), source, int (absentWord typ)) in
        (match pats with [] -> Ok (Some (A.Var cmp, [cmp, expression], vg1)) | [pat] ->
          let value, bindings, gen = match ptype with Some typ' when spare typ -> let id, gen = A.freshVar vg1 in A.Var id, [id, A.TypedAtom (source, typ')], gen | _ -> source, [], vg1 in
          let* inner = buildPatternComparison pat value ptype gen in
          (match inner with None -> Ok (Some (A.Var cmp, [cmp, expression], vg1))
          | Some (condition, innerBindings, gen) -> let id, gen = A.freshVar gen in
            Ok (Some (A.Var id, [cmp, expression] @ bindings @ innerBindings @ [id, A.Prim (A.And, A.Var cmp, condition)], gen)))
        | _ -> Crash.crash "Unboxed two-case sum must have one payload field")
      else if Option.is_some (transparent typ) then
        (match pats with [pat] -> buildPatternComparison pat source ptype gen | _ -> Crash.crash "Transparent sum must have one field")
      else if hasPayload name then
        let tagId, gen = A.freshVar gen in let cmp, gen = A.freshVar gen in
        let bindings = [tagId, A.TupleGet (source, 0); cmp, A.Prim (A.Eq, A.Var tagId, int (Int64.of_int tag))] in
        (match pats with [] -> Ok (Some (A.Var cmp, bindings, gen)) | _ ->
          let pat = match pats with [pat] -> pat | _ -> C.PTuple pats in
          let value, gen = A.freshVar gen in
          let* inner = buildPatternComparison pat (A.Var value) ptype gen in
          match inner with None -> Ok (Some (A.Var cmp, bindings, gen))
          | Some (condition, innerBindings, gen) -> let id, gen = A.freshVar gen in
            Ok (Some (A.Var id, bindings @ [value, A.TupleGet (source, 1)] @ innerBindings @ [id, A.Prim (A.And, A.Var cmp, condition)], gen)))
      else (match pats with [] -> single (A.Prim (A.Eq, source, int (Int64.of_int tag))) gen | _ -> Error "Type checking accepted fields for a nullary constructor"))
  | C.PTuple pats ->
    let rec andAll conditions gen bindings = match conditions with [] -> Error "Empty conditions list" | [condition] -> Ok (condition, bindings, gen)
    | first :: rest -> let* condition, bindings, gen = andAll rest gen bindings in let id, gen = A.freshVar gen in
      Ok (A.Var id, bindings @ [id, A.Prim (A.And, first, condition)], gen) in
    let rec loop pats index gen bindings conditions = match pats with [] ->
      if conditions = [] then Ok None else let* condition, bindings, gen = andAll conditions gen bindings in Ok (Some (condition, bindings, gen))
    | pat :: rest -> let value, vg1 = A.freshVar gen in
      let ptype = match typ with AST.TTuple types -> List.nth_opt types index | _ -> None in
      let* inner = buildPatternComparison pat (A.Var value) ptype vg1 in
      let bindings = bindings @ [value, A.TupleGet (source, index)] in
      match inner with None -> loop rest (index + 1) vg1 bindings conditions
      | Some (condition, innerBindings, gen) -> loop rest (index + 1) gen (bindings @ innerBindings) (conditions @ [condition]) in
    loop pats 0 gen [] []
  | C.PList [] -> single (A.Prim (A.Eq, source, int 0L)) gen
  | C.PList pats | C.PListCons (pats, _) ->
    if pats = [] then Ok None else
    let length, gen = A.freshVar gen in let cmp, gen = A.freshVar gen in
    let op = match pat with C.PList _ -> A.Eq | _ -> A.Gte in
    Ok (Some (A.Var cmp, [length, A.Call (functionId "Darklang.Stdlib.List.__length_i64", [source]); cmp, A.Prim (op, A.Var length, int (Int64.of_int (List.length pats)))], gen))
  | C.PInt64 _ | C.PInt128Literal _ | C.PInt8Literal _ | C.PInt16Literal _ | C.PInt32Literal _
  | C.PUInt8Literal _ | C.PUInt16Literal _ | C.PUInt32Literal _ | C.PUInt64Literal _ | C.PUInt128Literal _ -> assert false
 in
 let comparisonParts gen = function None -> None, [], gen | Some (condition, bindings, gen) -> Some condition, bindings, gen in
 let nestedChecked ?(alwaysCollect=false) pat value elem env gen =
  let impossible = patternStaticallyCannotMatchType pat elem in
  let* comparison = if impossible && not alwaysCollect then Ok (Some (makeFalsePatternCondition gen)) else buildPatternComparison pat value (Some elem) gen in
  let condition, bindings, gen = comparisonParts gen comparison in
  let* env, nested, gen = if alwaysCollect || (patternBindsVariables pat && not impossible) then collect Nested pat value elem env [] gen else Ok (env, [], gen) in
  Ok (env, bindings, nested, condition, gen) in
 let combinedChecks conditions gen =
  let rec loop remaining previous bindings gen = match remaining, previous with
   | [], None -> A.BoolLiteral true, bindings, gen
   | [], Some condition -> condition, bindings, gen
   | condition :: rest, None -> loop rest (Some condition) bindings gen
   | condition :: rest, Some previous -> let id, gen = A.freshVar gen in loop rest (Some (A.Var id)) (bindings @ [id, A.Prim (A.And, previous, condition)]) gen in
  loop conditions None [] gen in
 let guardChecks conditions body elseExpr gen = match conditions with [] -> body, gen | _ ->
  let condition, bindings, gen = combinedChecks conditions gen in K.wrapBindings bindings (A.If (condition, body, elseExpr)), gen in
 let compileListPatternWithChecks pats source typ env body elseExpr gen =
  match typ with AST.TList elem ->
    (match pats with
    | [] -> let id, gen = A.freshVar gen in let* body, gen = anf body gen env in Ok (A.Let (id, A.Prim (A.Eq, source, int 0L), A.If (A.Var id, body, elseExpr)), gen)
    | [pat] ->
      let length, gen = A.freshVar gen in let check, gen = A.freshVar gen in
      let value, valueId, bindings, gen = singletonLoads elem source gen in
      let* matched, gen = match pat with
      | C.PVariable name -> anf body gen (R.BindingMap.add name (valueId, elem) env)
      | C.PWildcard -> anf body gen env
      | _ when numeric pat -> let check, gen = A.freshVar gen in let* body, gen = anf body gen env in
        Ok (A.Let (check, literalComparison pat value, A.If (A.Var check, body, elseExpr)), gen)
      | C.PTuple _ | C.PConstructor _ | C.PList _ | C.PListCons _ ->
        let* env, comparisonBindings, nested, condition, gen = nestedChecked ~alwaysCollect:true pat value elem env gen in
        let* body, gen = anf body gen env in let body = K.wrapBindings nested body in
        Ok (K.wrapBindings comparisonBindings (match condition with None -> body | Some condition -> A.If (condition, body, elseExpr)), gen)
      | _ -> Error ("Unsupported pattern in single-element list: " ^ fmt pat) in
      Ok (K.wrapBindings [length, A.Call (functionId "Darklang.Stdlib.List.__length_i64", [source]); check, A.Prim (A.Eq, A.Var length, int 1L)] (A.If (A.Var check, K.wrapBindings bindings matched, elseExpr)), gen)
    | _ ->
      let length, gen = A.freshVar gen in let check, gen = A.freshVar gen in let ptr, gen = A.freshVar gen in
      let lengthName = if elem = AST.TFloat64 then "Darklang.Stdlib.List.__lengthFloat" else "Darklang.Stdlib.List.__length_i64" in
      let headers = [length, A.Call (functionId lengthName, [source]); check, A.Prim (A.Eq, A.Var length, int (Int64.of_int (List.length pats))); ptr, A.Prim (A.BitAnd, source, int (-8L))] in
      let rec loop pats index env bindings conditions gen = match pats with [] -> Ok (env, bindings, conditions, gen)
      | pat :: rest ->
        let opt, gen = A.freshVar gen in let raw, gen = A.freshVar gen in let value, gen = A.freshVar gen in
        let getAt = if elem = AST.TFloat64 then "Darklang.Stdlib.List.__getAtFloat" else "Darklang.Stdlib.List.__getAtInt64" in
        let bindings = bindings @ [opt, A.Call (functionId getAt, [source; int (Int64.of_int index)]); raw, A.RawGet (A.Var opt, int 8L, if elem = AST.TFloat64 then Some elem else None); value, A.TypedAtom (A.Var raw, elem)] in
        let* env, bindings, conditions, gen = match pat with
        | C.PVariable name -> Ok (R.BindingMap.add name (value, elem) env, bindings, conditions, gen)
        | C.PWildcard -> Ok (env, bindings, conditions, gen)
        | _ when numeric pat -> let check, gen = A.freshVar gen in Ok (env, bindings @ [check, literalComparison pat (A.Var value)], conditions @ [A.Var check], gen)
        | C.PTuple _ | C.PList _ | C.PListCons _ | C.PConstructor _ ->
          let* env, comparisons, nested, condition, gen = nestedChecked pat (A.Var value) elem env gen in
          Ok (env, bindings @ comparisons @ nested, conditions @ Option.to_list condition, gen)
        | _ -> Error ("Unsupported pattern in list element: " ^ fmt pat) in loop rest (index + 1) env bindings conditions gen in
      let* env, bindings, conditions, gen = loop pats 0 env [] [] gen in let* body, gen = anf body gen env in
      let body, gen = guardChecks conditions body elseExpr gen in
      Ok (K.wrapBindings headers (A.If (A.Var check, K.wrapBindings bindings body, elseExpr)), gen))
  | _ -> Ok (elseExpr, gen) in
 let rec compileListConsPatternWithChecks pats tailPat source typ env body elseExpr gen =
  let* elem = match typ with AST.TList elem -> Ok elem | _ -> Error ("List cons pattern expects TList scrutinee, got " ^ P.typeToString typ) in
  let rec _extractTupleBindings pats source typ index env bindings gen =
   let types = match typ with AST.TTuple types -> types | _ -> Crash.crash ("Tuple head pattern expects tuple element type, got " ^ P.typeToString typ) in
   match pats with [] -> Ok (env, bindings, gen) | pat :: rest ->
    let raw, genRaw = A.freshVar gen in
    let elem = match List.nth_opt types index with Some elem -> elem | None -> Crash.crash (Printf.sprintf "Tuple head pattern arity mismatch: requested index %d, tuple has %d elements" index (List.length types)) in
    let typed, genTyped = A.freshVar genRaw in
    (match pat with C.PVariable name -> _extractTupleBindings rest source typ (index + 1) (R.BindingMap.add name (typed, elem) env) (bindings @ [raw, A.TupleGet (source, index); typed, A.TypedAtom (A.Var raw, elem)]) genTyped
    | C.PWildcard -> _extractTupleBindings rest source typ (index + 1) env (bindings @ [raw, A.TupleGet (source, index)]) genRaw
    | _ -> Error ("Nested pattern in tuple element not yet supported: " ^ fmt pat)) in
  let _tupleHeadPatternType elem pats = match elem with AST.TTuple types when List.length types = List.length pats -> Some elem
    | AST.TVar name -> Some (AST.TTuple (unknownTuple ("__tuple_elem_" ^ name ^ "_") pats))
    | AST.TNever -> Some (AST.TTuple (unknownTuple "__tuple_elem_runtime_error_" pats)) | _ -> None in
  match pats with
  | [] -> (match tailPat with C.PVariable name -> let id, gen = A.freshVar gen in let* body, gen = anf body gen (R.BindingMap.add name (id, typ) env) in Ok (A.Let (id, A.Atom source, body), gen)
    | C.PWildcard -> anf body gen env | _ -> Error "Tail pattern in list cons must be variable or wildcard")
  | [pat] when (match pat with C.PList _ | C.PListCons _ | C.PConstructor _ -> false | _ -> true) && (match tailPat with C.PVariable _ | C.PWildcard -> true | _ -> false) ->
    let notEmpty, gen = A.freshVar gen in let tag, gen = A.freshVar gen in let ptr, gen = A.freshVar gen in let isSingle, gen = A.freshVar gen in
    let compileBranch isLegacy gen =
      let node, gen = A.freshVar gen in
      let value, _, bindings, gen = unwrapLeaf elem (A.Var node) gen [node, A.RawGet (A.Var ptr, int (if isLegacy then 0L else 16L), None)] in
      let typedHead, gen = A.freshVar gen in let bindings = bindings @ [typedHead, A.TypedAtom (value, elem)] in
      let rawTail, gen = A.freshVar gen in let typedTail, gen = A.freshVar gen in
      let tailBindings = [rawTail, (if isLegacy then A.Atom (int 0L) else tail source); typedTail, A.TypedAtom (A.Var rawTail, typ)] in
      let* env, tupleBindings, gen, guard = match pat with
      | C.PVariable name -> Ok (R.BindingMap.add name (typedHead, elem) env, [], gen, None)
      | C.PWildcard -> Ok (env, [], gen, None)
      | C.PTuple _ | C.PConstructor _ | C.PList _ | C.PListCons _ ->
        let impossible = patternStaticallyCannotMatchType pat elem in
        let* comparison = if impossible then Ok (Some (makeFalsePatternCondition gen)) else buildPatternComparison pat (A.Var typedHead) (Some elem) gen in
        let condition, comparisonBindings, gen = comparisonParts gen comparison in
        let guard, gen = match condition with None -> None, gen | Some condition -> let id, gen = A.freshVar gen in Some (id, A.Atom condition), gen in
        let* env, nested, gen = if patternBindsVariables pat && not impossible then collect Nested pat (A.Var typedHead) elem env [] gen else Ok (env, [], gen) in
        Ok (env, comparisonBindings @ nested, gen, guard)
      | _ when numeric pat -> let id, gen = A.freshVar gen in Ok (env, [], gen, Some (id, literalComparison pat (A.Var typedHead)))
      | _ -> Error ("Unsupported head pattern in list cons: " ^ fmt pat) in
      let* env = match tailPat with C.PVariable name -> Ok (R.BindingMap.add name (typedTail, typ) env) | C.PWildcard -> Ok env | _ -> Error "Tail pattern must be variable or wildcard" in
      let* body, gen = anf body gen env in let body = K.wrapBindings tailBindings body in
      let body = match guard with None -> body | Some (id, expression) -> A.Let (id, expression, A.If (A.Var id, body, elseExpr)) in
      Ok (K.wrapBindings bindings (K.wrapBindings tupleBindings body), gen) in
    let* single, gen = compileBranch true gen in let* deep, gen = compileBranch false gen in
    let inner = K.wrapBindings [tag, A.Prim (A.BitAnd, source, int 7L); ptr, A.Prim (A.BitAnd, source, int (-8L)); isSingle, A.Atom (A.BoolLiteral false)] (A.If (A.Var isSingle, single, deep)) in
    Ok (A.Let (notEmpty, A.Prim (A.Neq, source, int 0L), A.If (A.Var notEmpty, inner, elseExpr)), gen)
  | _ ->
    let length, gen = A.freshVar gen in let check, gen = A.freshVar gen in
    let lengthName = if elem = AST.TFloat64 then "Darklang.Stdlib.List.__lengthFloat" else "Darklang.Stdlib.List.__length_i64" in
    let headers = [length, A.Call (functionId lengthName, [source]); check, A.Prim (A.Gte, A.Var length, int (Int64.of_int (List.length pats)))] in
    let initial, gen = A.freshVar gen in
    let rec loop pats current env bindings conditions gen = match pats with [] -> Ok (env, bindings, current, conditions, gen)
    | pat :: rest ->
      let rawHead, gen = A.freshVar gen in let rawTail, gen = A.freshVar gen in let typedHead, gen = A.freshVar gen in let typedTail, gen = A.freshVar gen in
      let bindings = bindings @ [rawHead, head elem (A.Var current); typedHead, A.TypedAtom (A.Var rawHead, elem); rawTail, tail (A.Var current); typedTail, A.TypedAtom (A.Var rawTail, typ)] in
      let* env, bindings, conditions, gen = match pat with
      | C.PVariable name -> Ok (R.BindingMap.add name (typedHead, elem) env, bindings, conditions, gen)
      | C.PWildcard -> Ok (env, bindings, conditions, gen)
      | _ when numeric pat -> let id, gen = A.freshVar gen in Ok (env, bindings @ [id, literalComparison pat (A.Var typedHead)], conditions @ [A.Var id], gen)
      | C.PConstructor _ | C.PList _ | C.PListCons _ ->
        let* env, comparisonBindings, nested, condition, gen = nestedChecked pat (A.Var typedHead) elem env gen in
        Ok (env, bindings @ comparisonBindings @ nested, conditions @ Option.to_list condition, gen)
      | _ -> Error ("Unsupported head pattern in multi-element list cons: " ^ fmt pat) in
      loop rest typedTail env bindings conditions gen in
    let* env, headBindings, finalTail, headConditions, gen = loop pats initial env [initial, A.Atom source] [] gen in
    let* body, tailBindings, tailConditions, gen = match tailPat with
    | C.PList pats -> let* body, gen = compileListPatternWithChecks pats (A.Var finalTail) typ env body elseExpr gen in Ok (body, [], [], gen)
    | C.PListCons (pats, tailPat) -> let* body, gen = compileListConsPatternWithChecks pats tailPat (A.Var finalTail) typ env body elseExpr gen in Ok (body, [], [], gen)
    | C.PVariable name -> let* body, gen = anf body gen (R.BindingMap.add name (finalTail, typ) env) in Ok (body, [], [], gen)
    | C.PWildcard -> let* body, gen = anf body gen env in Ok (body, [], [], gen)
    | _ ->
      let* env, comparisonBindings, nested, condition, gen = nestedChecked tailPat (A.Var finalTail) typ env gen in
      let* body, gen = anf body gen env in Ok (body, comparisonBindings @ nested, Option.to_list condition, gen) in
    let body, gen = guardChecks (headConditions @ tailConditions) body elseExpr gen in
    Ok (K.wrapBindings headers (A.If (A.Var check, K.wrapBindings (headBindings @ tailBindings) body, elseExpr)), gen)
 in
 let prependBindings bindings = function [] -> [] | (first, condition) :: rest -> (bindings @ first, condition) :: rest in
 let rec buildPatternStages pat source patType gen =
  let typ = Option.value ~default:scrutType patType in
  let flat () = let* comparison = buildPatternComparison pat source patType gen in
    Ok (match comparison with None -> [], gen | Some (condition, bindings, gen) -> [bindings, condition], gen) in
  match pat with
  | C.PConstructor (id, pats) when pats <> [] && not (List.for_all patternAlwaysMatches pats) ->
    (match variant id typ with
    | Some (name, params, _, fields) when unboxed typ ->
      (match pats with [pat] ->
        let cmp, gen = A.freshVar gen in
        let stage = [cmp, A.Prim (A.Neq, source, int (absentWord typ))], A.Var cmp in
        let ptype = match typ with AST.TSum (_, args) ->
          (match P.nullablePointerSumPayloadType name args sums.P.cases with Some typ -> Some typ | None ->
            match fields with [field] when List.length params = List.length args -> Some (substituteTypeParams (M.of_seq (List.to_seq (List.combine params args))) field)
            | _ -> Crash.crash "Unboxed two-case sum must have one payload field") | _ -> None in
        let source, bindings, gen = match ptype with Some ptype when spare typ -> let id, gen = A.freshVar gen in A.Var id, [id, A.TypedAtom (source, ptype)], gen | _ -> source, [], gen in
        let* inner, gen = buildPatternStages pat source ptype gen in Ok (stage :: prependBindings bindings inner, gen)
      | _ -> Crash.crash "Unboxed two-case sum must have one payload field")
    | Some _ when Option.is_some (transparent typ) -> (match pats with [pat] -> buildPatternStages pat source (transparent typ) gen | _ -> Crash.crash "Transparent sum must have one field")
    | Some (name, params, tag, fields) when hasPayload name ->
      let fields = match typ with AST.TSum (_, args) when List.length params = List.length args -> List.map (substituteTypeParams (M.of_seq (List.to_seq (List.combine params args)))) fields | _ -> fields in
      let inner, ptype = payload pats fields in
      let tagId, gen = A.freshVar gen in let cmp, gen = A.freshVar gen in let value, gen = A.freshVar gen in
      let stage = [tagId, A.TupleGet (source, 0); cmp, A.Prim (A.Eq, A.Var tagId, int (Int64.of_int tag))], A.Var cmp in
      let* stages, gen = buildPatternStages inner (A.Var value) (Some ptype) gen in
      Ok (stage :: prependBindings [value, A.TupleGet (source, 1)] stages, gen)
    | _ -> flat ())
  | C.PTuple pats ->
    let rec elements pats index gen stages = match pats with [] -> Ok (stages, gen) | pat :: rest ->
      let ptype = match typ with AST.TTuple types -> List.nth_opt types index | _ -> None in
      let id, gen = A.freshVar gen in let* inner, gen = buildPatternStages pat (A.Var id) ptype gen in
      elements rest (index + 1) gen (stages @ prependBindings [id, A.TupleGet (source, index)] inner) in elements pats 0 gen []
  | C.PList [] -> let id, gen = A.freshVar gen in Ok ([[id, A.Prim (A.Eq, source, int 0L)], A.Var id], gen)
  | C.PList _ | C.PListCons _ ->
    let pats, tailPat, exact = match pat with C.PList pats -> pats, None, true | C.PListCons (pats, tail) -> pats, Some tail, false | _ -> assert false in
    let elem = match typ with AST.TList elem -> elem | _ -> AST.TVar "__list_elem_unknown" in let listType = AST.TList elem in
    let length, gen = A.freshVar gen in let cmp, gen = A.freshVar gen in
    let stage = [length, A.Call (functionId "Darklang.Stdlib.List.__length_i64", [source]); cmp, A.Prim ((if exact then A.Eq else A.Gte), A.Var length, int (Int64.of_int (List.length pats)))], A.Var cmp in
    let rec heads pats current gen stages = match pats with
    | [] -> (match tailPat with Some pat when not (patternAlwaysMatches pat) -> let* inner, gen = buildPatternStages pat current (Some listType) gen in Ok (stages @ inner, gen) | _ -> Ok (stages, gen))
    | pat :: rest ->
      let rawHead, gen = A.freshVar gen in let typedHead, gen = A.freshVar gen in let rawTail, gen = A.freshVar gen in let typedTail, gen = A.freshVar gen in
      let headLoads = [rawHead, head elem current; typedHead, A.TypedAtom (A.Var rawHead, elem)] in
      let tailLoads = [rawTail, tail current; typedTail, A.TypedAtom (A.Var rawTail, listType)] in
      let* inner, gen = buildPatternStages pat (A.Var typedHead) (Some elem) gen in
      let stages = stages @ prependBindings headLoads inner in
      let needsTail = rest <> [] || Option.fold ~none:false ~some:(fun pat -> not (patternAlwaysMatches pat)) tailPat in
      heads rest (A.Var typedTail) gen (if needsTail then stages @ [tailLoads, A.BoolLiteral true] else stages) in
    heads pats source gen [stage]
  | _ -> flat () in
 let stagesToIf stages thenExpr elseExpr gen = match stages with
  | [] -> thenExpr, gen
  | [bindings, condition] -> K.wrapBindings bindings (A.If (condition, thenExpr, elseExpr)), gen
  | _ ->
    let result, gen = A.freshVar gen in
    let rec entry = function [] -> A.Jump (result, A.BoolLiteral true)
    | [bindings, condition] -> K.wrapBindings bindings (A.Jump (result, condition))
    | (bindings, condition) :: rest -> K.wrapBindings bindings (A.If (condition, entry rest, A.Jump (result, A.BoolLiteral false))) in
    A.Join ({A.id = result; typ = AST.TBool}, A.If (A.Var result, thenExpr, elseExpr), entry stages), gen in
 let buildPatternGroupComparison pats source gen =
  let rec buildOr pats previous bindings gen = match pats with
   | [] -> Ok (Option.map (fun condition -> condition, bindings, gen) previous)
   | pat :: rest ->
     let* comparison = if patternStaticallyCannotMatchType pat scrutType then Ok (Some (makeFalsePatternCondition gen)) else buildPatternComparison pat source (Some scrutType) gen in
     match comparison with None -> Ok None | Some (condition, extra, gen) ->
      (match previous with None -> buildOr rest (Some condition) (extra @ bindings) gen
      | Some previous -> let id, gen = A.freshVar gen in buildOr rest (Some (A.Var id)) (bindings @ extra @ [id, A.Prim (A.Or, previous, condition)]) gen) in
  match pats with [] -> Ok None | [pat] -> if patternStaticallyCannotMatchType pat scrutType then Ok (Some (makeFalsePatternCondition gen)) else buildPatternComparison pat source (Some scrutType) gen
  | _ -> buildOr pats None [] gen in
 let constructorPatternCoverage pat = match scrutType, pat with
  | AST.TSum (name, _), C.PConstructor (id, pats) -> (match variant id scrutType with
    | Some (owner, _, tag, fields) when name = owner && List.length pats = List.length fields && List.for_all patternAlwaysMatches pats -> Some tag | _ -> None)
  | _ -> None in
 let constructorMatchIsExhaustive cases =
  let covered = List.fold_left (fun covered case -> match covered, case.C.guard with Some covered, None ->
    List.fold_left (fun covered pat -> match covered, constructorPatternCoverage pat with Some covered, Some tag -> Some (Tags.add tag covered) | _ -> None) (Some covered) (NonEmptyList.toList case.C.patterns)
    | _ -> None) (Some Tags.empty) cases in
  match scrutType, covered with AST.TSum (name, _), Some covered ->
    let all = M.fold (fun _ (owner, _, tag, _) acc -> if owner = name then Tags.add tag acc else acc) variants Tags.empty in
    not (Tags.is_empty all) && Tags.equal covered all | _ -> false in
 let escapeForRuntimeError text =
  let result = Buffer.create (String.length text) in String.iter (fun char -> Buffer.add_string result (match char with '\\' -> "\\\\" | '"' -> "\\\"" | '\n' -> "\\n" | '\r' -> "\\r" | '\t' -> "\\t" | _ -> String.make 1 char)) text; Buffer.contents result in
 let unsigned n = Z.to_string (if n < 0L then Z.add (Z.of_int64 n) (Z.shift_left Z.one 64) else Z.of_int64 n) in
 let rec formatMatchValueForError value =
  let rec formatAll expressions acc = match expressions with [] -> Some (List.rev acc) | expr :: rest -> Option.bind (formatMatchValueForError expr) (fun formatted -> formatAll rest (formatted :: acc)) in
  let sequence start finish values = Option.map (fun values -> start ^ String.concat ", " values ^ finish) (formatAll values []) in
  match value with
  | C.Int64Literal n -> Some (Int64.to_string n)
  | C.Int128Literal n -> Some (P.int128ToCanonicalString n)
  | C.Int8Literal n | C.Int16Literal n | C.UInt8Literal n | C.UInt16Literal n -> Some (string_of_int n)
  | C.Int32Literal n -> Some (Int32.to_string n)
  | C.UInt32Literal n -> Some (Int64.to_string n)
  | C.UInt64Literal n -> Some (unsigned n)
  | C.UInt128Literal n -> Some (P.uint128ToCanonicalString n)
  | C.BoolLiteral n -> Some (if n then "true" else "false")
  | C.FloatLiteral n -> Some (HostFloat.roundTrip n)
  | C.UnitLiteral -> Some "()"
  | C.StringLiteral value -> Some ("\"" ^ escapeForRuntimeError value ^ "\"")
  | C.CharLiteral value -> Some ("'" ^ escapeForRuntimeError value ^ "'")
  | C.TupleLiteral values -> sequence "(" ")" (C.tupleElementsToList values)
  | C.ListLiteral values -> sequence "[" "]" values
  | C.Constructor (reference, fields) ->
    let name = Option.value ~default:"<unknown-type>" (P.tryFindSumTypeNameById reference.C.typeId typeNames) in
    let tag = constructorTag reference.C.constructorId in
    let fullName = match List.find_opt (fun (_, (owner, _, tag', _)) -> owner = name && tag = tag') (M.bindings variants) with Some (name, _) -> name | None -> name ^ ".<tag " ^ string_of_int tag ^ ">" in
    if fields = [] then Some fullName else sequence (fullName ^ "(") ")" fields
  | _ -> None in
 let makeNoMatchingCaseFallback gen =
  let value = Option.value ~default:"<unknown>" (formatMatchValueForError scrutinee) in
  let id, gen = A.freshVar gen in
  A.Let (id, A.RuntimeError ("Non-exhaustive match: No matching case found for value " ^ value ^ " in match expression"), A.Return (A.Var id)), gen in
 let rec buildChain remaining gen = match remaining with
  | [] -> Error ("Non-exhaustive pattern match for " ^ P.typeToString scrutType)
  | case :: rest when case.C.patterns.NonEmptyList.tail <> [] ->
    let expanded = NonEmptyList.toList case.C.patterns |> List.filter (fun pat -> not (patternStaticallyCannotMatchType pat scrutType)) |> List.map (fun pat -> {case with C.patterns = NonEmptyList.singleton pat}) in
    buildChain (expanded @ rest) gen
  | [case] ->
    let pat = case.C.patterns.NonEmptyList.head in let body = case.C.body in
    let fallback, vg1 = makeNoMatchingCaseFallback gen in
    let compileBody gen = match case.C.guard, pat with
     | None, C.PList (_ :: _ as pats) when not (listArmNeedsStages pat) -> compileListPatternWithChecks pats scrutineeAtom scrutType env body fallback gen
     | None, C.PListCons (pats, tailPat) when not (listArmNeedsStages pat) -> compileListConsPatternWithChecks pats tailPat scrutineeAtom scrutType env body fallback gen
     | None, _ -> extractAndCompileBody pat body scrutineeAtom scrutType env gen
     | Some guard, _ -> extractAndCompileBodyWithGuard pat guard body scrutineeAtom scrutType env gen fallback in
    let skip = match case.C.guard, pat with None, (C.PList (_ :: _) | C.PListCons _) -> not (listArmNeedsStages pat) | _ -> false in
    if skip then compileBody vg1
    else if case.C.guard = None && Option.is_some (constructorPatternCoverage pat) && constructorMatchIsExhaustive cases then extractAndCompileBody pat body scrutineeAtom scrutType env vg1
    else if patternStaticallyCannotMatchType pat scrutType then let* _, gen = compileBody vg1 in Ok (fallback, gen)
    else let* stages, vg2 = buildPatternStages pat scrutineeAtom (Some scrutType) vg1 in
      if stages = [] then compileBody vg1 else let* body, gen = compileBody vg2 in Ok (stagesToIf stages body fallback gen)
  | case :: rest ->
    let pat = case.C.patterns.NonEmptyList.head in let body = case.C.body in
    if patternAlwaysMatches pat then (match case.C.guard with None -> extractAndCompileBody pat body scrutineeAtom scrutType env gen
    | Some guard -> let* elseExpr, gen = buildChain rest gen in extractAndCompileBodyWithGuard pat guard body scrutineeAtom scrutType env gen elseExpr)
    else match pat, case.C.guard with
    | C.PList (_ :: _ as pats), None when not (listArmNeedsStages pat) -> let* elseExpr, gen = buildChain rest gen in compileListPatternWithChecks pats scrutineeAtom scrutType env body elseExpr gen
    | C.PListCons (pats, tailPat), None when not (listArmNeedsStages pat) -> let* elseExpr, gen = buildChain rest gen in compileListConsPatternWithChecks pats tailPat scrutineeAtom scrutType env body elseExpr gen
    | _ when case.C.patterns.NonEmptyList.tail = [] && not (patternStaticallyCannotMatchType pat scrutType) ->
      let* stages, gen = buildPatternStages pat scrutineeAtom (Some scrutType) gen in
      (match stages, case.C.guard with
      | [], None -> extractAndCompileBody pat body scrutineeAtom scrutType env gen
      | [], Some guard -> let* elseExpr, gen = buildChain rest gen in extractAndCompileBodyWithGuard pat guard body scrutineeAtom scrutType env gen elseExpr
      | _, None -> let* body, gen = extractAndCompileBody pat body scrutineeAtom scrutType env gen in let* elseExpr, gen = buildChain rest gen in Ok (stagesToIf stages body elseExpr gen)
      | _, Some guard -> let* elseExpr, gen = buildChain rest gen in let* body, gen = extractAndCompileBodyWithGuard pat guard body scrutineeAtom scrutType env gen elseExpr in Ok (stagesToIf stages body elseExpr gen))
    | _ -> let* comparison = buildPatternGroupComparison (NonEmptyList.toList case.C.patterns) scrutineeAtom gen in
      (match comparison, case.C.guard with
      | None, None -> extractAndCompileBody pat body scrutineeAtom scrutType env gen
      | None, Some guard -> let* elseExpr, gen = buildChain rest gen in extractAndCompileBodyWithGuard pat guard body scrutineeAtom scrutType env gen elseExpr
      | Some (condition, bindings, gen), None -> let* body, gen = extractAndCompileBody pat body scrutineeAtom scrutType env gen in let* elseExpr, gen = buildChain rest gen in Ok (K.wrapBindings bindings (A.If (condition, body, elseExpr)), gen)
      | Some (condition, bindings, gen), Some guard -> let* elseExpr, gen = buildChain rest gen in let* body, gen = extractAndCompileBodyWithGuard pat guard body scrutineeAtom scrutType env gen elseExpr in Ok (K.wrapBindings bindings (A.If (condition, body, elseExpr)), gen)) in
 let* chain, gen = buildChain cases gen in
 Ok (K.bindReturns scrutineeExpr (fun _ -> K.wrapBindings postBindings chain), gen)
