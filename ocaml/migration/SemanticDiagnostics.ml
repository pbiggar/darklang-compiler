open Dark_compiler
let union = SemanticJson.union
let str = SemanticJson.string
let typ = SemanticAST.semanticType
let list encode values = `List (List.map encode values)
let nonEmpty encode (value : 'a NonEmptyList.t) = SemanticJson.record "NonEmptyList" ["Head", encode value.NonEmptyList.head; "Tail", list encode value.NonEmptyList.tail]
let qualified value = union "QualifiedName" "QualifiedName" [nonEmpty str (NonEmptyList.fromList (NameResolution.qualifiedNameSegments value))]
let context value = union "ResolutionContext" (match value with NameResolution.Value -> "Value" | NameResolution.Callable -> "Callable" | NameResolution.Constructor -> "Constructor" | NameResolution.Type -> "Type") []
let namespace = function
 | NameResolution.RootNamespace -> union "NamespaceIdentity" "RootNamespace" []
 | NameResolution.BuiltinNamespace -> union "NamespaceIdentity" "BuiltinNamespace" []
 | NameResolution.ModuleNamespace path -> union "NamespaceIdentity" "ModuleNamespace" [nonEmpty str path]
 | NameResolution.PackageNamespace (owner, path) -> union "NamespaceIdentity" "PackageNamespace" [str owner; list str path]
let identity = function
 | NameResolution.LocalValue name -> union "SymbolIdentity" "LocalValue" [str name]
 | NameResolution.ModuleValue (owner, name) -> union "SymbolIdentity" "ModuleValue" [namespace owner; str name]
 | NameResolution.PackageValue (owner, name) -> union "SymbolIdentity" "PackageValue" [namespace owner; str name]
 | NameResolution.BuiltinValue (name, version) -> union "SymbolIdentity" "BuiltinValue" [str name; SemanticJson.int32 version]
 | NameResolution.ModuleFunction (owner, name, id) -> union "SymbolIdentity" "ModuleFunction" [namespace owner; str name; str id]
 | NameResolution.PackageFunction (owner, name, id) -> union "SymbolIdentity" "PackageFunction" [namespace owner; str name; str id]
 | NameResolution.BuiltinFunction (name, version) -> union "SymbolIdentity" "BuiltinFunction" [str name; SemanticJson.int32 version]
 | NameResolution.ConstructorSymbol (owner, name) -> union "SymbolIdentity" "ConstructorSymbol" [str owner; str name]
 | NameResolution.UserType name -> union "SymbolIdentity" "UserType" [str name]
 | NameResolution.BuiltinType name -> union "SymbolIdentity" "BuiltinType" [str name]
let resolution = function
 | NameResolution.InvalidQualifiedName (name, ctxt) -> union "ResolutionError" "InvalidQualifiedName" [str name; context ctxt]
 | NameResolution.UnresolvedName (name, ctxt) -> union "ResolutionError" "UnresolvedName" [qualified name; context ctxt]
 | NameResolution.AmbiguousReference (name, ctxt, values) -> union "ResolutionError" "AmbiguousReference" [qualified name; context ctxt; list identity values]
let typeError = function
 | CheckingDiagnostics.TypeMismatch (expected, actual, text) -> union "TypeError" "TypeMismatch" [typ expected; typ actual; str text]
 | CheckingDiagnostics.IfBranchTypeMismatch (left, right) -> union "TypeError" "IfBranchTypeMismatch" [typ left; typ right]
 | CheckingDiagnostics.UndefinedVariable name -> union "TypeError" "UndefinedVariable" [str name]
 | CheckingDiagnostics.UndefinedCallTarget name -> union "TypeError" "UndefinedCallTarget" [str name]
 | CheckingDiagnostics.MissingTypeAnnotation name -> union "TypeError" "MissingTypeAnnotation" [str name]
 | CheckingDiagnostics.InvalidOperation (name, args) -> union "TypeError" "InvalidOperation" [str name; list typ args]
 | CheckingDiagnostics.IncompatibleEqualityOperands (left, right) -> union "TypeError" "IncompatibleEqualityOperands" [typ left; typ right]
 | CheckingDiagnostics.IncompatibleOrderingOperands (left, right) -> union "TypeError" "IncompatibleOrderingOperands" [typ left; typ right]
 | CheckingDiagnostics.PolymorphicRecursion name -> union "TypeError" "PolymorphicRecursion" [str name]
 | CheckingDiagnostics.ResolutionFailure error -> union "TypeError" "ResolutionFailure" [resolution error]
 | CheckingDiagnostics.GenericError error -> union "TypeError" "GenericError" [str error]
let provenance = function
 | NameResolution.LexicalBinding value -> union "CandidateProvenance" "LexicalBinding" [str value]
 | NameResolution.SourceDeclaration value -> union "CandidateProvenance" "SourceDeclaration" [str value]
 | NameResolution.ModuleDeclaration value -> union "CandidateProvenance" "ModuleDeclaration" [str value]
 | NameResolution.PackageDeclaration value -> union "CandidateProvenance" "PackageDeclaration" [str value]
 | NameResolution.BuiltinRegistration value -> union "CandidateProvenance" "BuiltinRegistration" [str value]
 | NameResolution.CompilerExtension value -> union "CandidateProvenance" "CompilerExtension" [str value]
