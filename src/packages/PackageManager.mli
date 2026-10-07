(* PackageManager.mli - Hosted package resolution and persistent response cache. *)
[@@@warning "-30"]
type config={server:string;cachePath:string}
type resolvedSource={name:string;source:string}
type itemKind=PackageType|PackageValue|PackageFunction
type locatedEntity={kind:itemKind;hash:string;location:string;json:string}
type fetchResult=Found of string|Missing
val defaultServer : string
val defaultCachePath : unit -> string
val defaultConfig : unit -> config
val kindPath : itemKind -> string
val allKinds : itemKind list
val cacheRead : config -> string -> (fetchResult option,string) result
val cacheWrite : config -> string -> fetchResult -> (unit,string) result
val requestNetwork : PackageIO.client -> config -> string -> (fetchResult,string) result
val fetchByHash : PackageIO.client -> config -> string -> (fetchResult,string) result
val findByName : PackageIO.client -> config -> string -> (fetchResult,string) result
val parseHashJson : string -> (string,string) result
val locationName : Yojson.Raw.t -> (string,string) result
val resolvedName : Yojson.Raw.t -> (string,string) result
val renderType : Yojson.Raw.t -> (string,string) result
val renderLetPattern : Yojson.Raw.t -> (string,string) result
val renderMatchPattern : Yojson.Raw.t -> (string,string) result
val infixText : Yojson.Raw.t -> (string,string) result
val renderExpr : string list -> string -> Yojson.Raw.t -> (string,string) result
val parseLocatedEntity : itemKind -> string -> string -> (locatedEntity,string) result
val dependencyRefs : string -> ((string*string) list,string) result
val renderEntity : locatedEntity -> (resolvedSource,string) result
val candidatePrefixes : (string->bool) -> string list -> string list
val resolveWritten : config -> NameResolution.resolutionEnvironment -> Validation.validatedSourceFile list -> (resolvedSource list,string) result
