(* Preserve complete typed name-resolution and recursive-group failure descriptions. *)
[@@@warning "-42"]

open Dark_compiler
open StructuralValue
module N = NameResolution

let text value = Text value
let integer value = Scalar (string_of_int value)
let strings values = Sequence (List.map text values)

let nonEmpty values =
  Record
    [
      ("Head", text (NonEmptyList.head values));
      ("Tail", strings values.NonEmptyList.tail);
    ]

let qualified name =
  Union
    ( "QualifiedName",
      [ nonEmpty (NonEmptyList.fromList (N.qualifiedNameSegments name)) ] )

let context value =
  Union
    ( (match value with
      | N.Value -> "Value"
      | N.Callable -> "Callable"
      | N.Constructor -> "Constructor"
      | N.Type -> "Type"),
      [] )

let namespace = function
  | N.RootNamespace -> Union ("RootNamespace", [])
  | N.ModuleNamespace path -> Union ("ModuleNamespace", [ nonEmpty path ])
  | N.PackageNamespace (owner, modules) ->
      Union ("PackageNamespace", [ text owner; strings modules ])
  | N.BuiltinNamespace -> Union ("BuiltinNamespace", [])

let identity = function
  | N.LocalValue name -> Union ("LocalValue", [ text name ])
  | N.ModuleValue (scope, name) ->
      Union ("ModuleValue", [ namespace scope; text name ])
  | N.PackageValue (scope, name) ->
      Union ("PackageValue", [ namespace scope; text name ])
  | N.BuiltinValue (name, version) ->
      Union ("BuiltinValue", [ text name; integer version ])
  | N.ModuleFunction (scope, name, declaration) ->
      Union ("ModuleFunction", [ namespace scope; text name; text declaration ])
  | N.PackageFunction (scope, name, declaration) ->
      Union ("PackageFunction", [ namespace scope; text name; text declaration ])
  | N.BuiltinFunction (name, version) ->
      Union ("BuiltinFunction", [ text name; integer version ])
  | N.ConstructorSymbol (owner, name) ->
      Union ("ConstructorSymbol", [ text owner; text name ])
  | N.UserType name -> Union ("UserType", [ text name ])
  | N.BuiltinType name -> Union ("BuiltinType", [ text name ])

let provenance = function
  | N.LexicalBinding name -> Union ("LexicalBinding", [ text name ])
  | N.SourceDeclaration name -> Union ("SourceDeclaration", [ text name ])
  | N.ModuleDeclaration name -> Union ("ModuleDeclaration", [ text name ])
  | N.PackageDeclaration name -> Union ("PackageDeclaration", [ text name ])
  | N.BuiltinRegistration name -> Union ("BuiltinRegistration", [ text name ])
  | N.CompilerExtension name -> Union ("CompilerExtension", [ text name ])

let successfulResolution (value : N.successfulResolution) =
  Record
    [
      ("OriginalName", qualified value.N.originalName);
      ("Context", context value.N.context);
      ("Identity", identity value.N.identity);
      ("Provenance", provenance value.N.provenance);
    ]

let resolutionError = function
  | N.InvalidQualifiedName (name, scope) ->
      Union ("InvalidQualifiedName", [ text name; context scope ])
  | N.UnresolvedName (name, scope) ->
      Union ("UnresolvedName", [ qualified name; context scope ])
  | N.AmbiguousReference (name, scope, candidates) ->
      Union
        ( "AmbiguousReference",
          [
            qualified name;
            context scope;
            Sequence (List.map identity candidates);
          ] )

let kind value =
  Union
    ( (match value with
      | AST.TopLevelFunctionMember -> "TopLevelFunctionMember"
      | AST.NamedLocalFunctionMember -> "NamedLocalFunctionMember"
      | AST.DirectLambdaValueMember -> "DirectLambdaValueMember"),
      [] )

let availability value =
  Union
    ( (match value with
      | AST.OrdinaryBinding -> "OrdinaryBinding"
      | AST.SelfRecursiveMember -> "SelfRecursiveMember"
      | AST.MutualRecursiveMember -> "MutualRecursiveMember"
      | AST.CompletedGroupMember -> "CompletedGroupMember"
      | AST.ImportedGroupMember -> "ImportedGroupMember"),
      [] )

let parsed (value : AST.parsedRecursiveMember) =
  Record
    [
      ("Binding", AST.DiagnosticFormatting.binding value.AST.binding);
      ("Boundary", AST.DiagnosticFormatting.scope value.AST.boundary);
      ("Member", AST.DiagnosticFormatting.memberId value.AST.member);
      ("SourceName", text value.AST.sourceName);
      ("Kind", kind value.AST.kind);
    ]

let resolvedRecursiveMember (value : AST.resolvedRecursiveMember) =
  Record
    [
      ("Parsed", parsed value.AST.parsed);
      ("Group", AST.DiagnosticFormatting.group value.AST.group);
      ("GroupIndex", integer value.AST.groupIndex);
      ("Availability", availability value.AST.availability);
    ]
