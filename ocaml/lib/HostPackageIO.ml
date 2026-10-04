(* Native host boundaries for the package resolver's SQLite cache and HTTP. *)
external cacheRead : string -> string -> (int * string) option = "dark_package_cache_read"
external cacheWrite : string -> string -> int -> string -> unit = "dark_package_cache_write"
external resolveUrl : string -> string -> string = "dark_package_resolve_url"
type client
external create : unit -> client = "dark_package_http_create"
external dispose : client -> unit = "dark_package_http_dispose"
external getBytes : client -> string -> int * string * string option = "dark_package_http_get"
external decode : string -> string -> string = "dark_package_decode"
let encodingName name=match String.lowercase_ascii name with
 |"ansi_x3.4-1968"|"ansi_x3.4-1986"|"ascii"|"cp367"|"csascii"|"ibm367"|"iso-ir-6"|"iso646-us"|"iso_646.irv:1991"|"us"|"us-ascii"->"ASCII"
 |"cp819"|"csisolatin1"|"ibm819"|"iso-8859-1"|"iso-ir-100"|"iso8859-1"|"iso_8859-1"|"iso_8859-1:1987"|"l1"|"latin1"->"ISO-8859-1"
 |"csunicode11utf7"|"unicode-1-1-utf-7"|"unicode-2-0-utf-7"|"utf-7"|"x-unicode-1-1-utf-7"|"x-unicode-2-0-utf-7"->failwith "Support for UTF-7 is disabled. See https://aka.ms/dotnet-warnings/SYSLIB0001 for more information."
 |"iso-10646-ucs-2"|"ucs-2"|"unicode"|"utf-16"|"utf-16le"->"UTF-16LE"
 |"unicode-1-1-utf-8"|"unicode-2-0-utf-8"|"utf-8"|"x-unicode-1-1-utf-8"|"x-unicode-2-0-utf-8"->"UTF-8"
 |"unicodefffe"|"utf-16be"->"UTF-16BE"
 |"utf-32"|"utf-32le"->"UTF-32LE"
 |"utf-32be"->"UTF-32BE"
 |_->invalid_arg "The character set provided in ContentType is invalid. Cannot read content as string using an invalid character set."
let decodeContent contentType bytes=
 if bytes="" then "" else
 let charset=Option.bind contentType (fun contentType->
  List.find_map (fun part->match String.split_on_char '=' part with
   |[key;value] when String.lowercase_ascii (String.trim key)="charset"->
    let value=String.trim value in
    let quoted=String.length value>=2 && value.[0]='"' && value.[String.length value-1]='"' in
    if quoted then Some (if String.length value>2 then String.sub value 1 (String.length value-2) else value)
    else if String.exists (fun c->Char.code c<=32 || Char.code c>=127 || String.contains "()<>@,;:\\\"/[]?={}" c) value then None
    else Some value
   |_->None) (String.split_on_char ';' contentType)) in
 let strip prefix=if String.starts_with ~prefix bytes then String.sub bytes (String.length prefix) (String.length bytes-String.length prefix) else bytes in
 let charset,bytes=match charset with
 |Some name->let encoding=encodingName name in let preamble=match encoding with "UTF-8"->"\239\187\191"|"UTF-16LE"->"\255\254"|"UTF-16BE"->"\254\255"|"UTF-32LE"->"\255\254\000\000"|"UTF-32BE"->"\000\000\254\255"|_->"" in encoding,strip preamble
 |None->if String.starts_with ~prefix:"\255\254\000\000" bytes then "UTF-32LE",strip "\255\254\000\000" else if String.starts_with ~prefix:"\239\187\191" bytes then "UTF-8",strip "\239\187\191" else if String.starts_with ~prefix:"\255\254" bytes then "UTF-16LE",strip "\255\254" else if String.starts_with ~prefix:"\254\255" bytes then "UTF-16BE",strip "\254\255" else "UTF-8",bytes in
 if charset="ASCII" then String.map (fun c->if Char.code c>=128 then '?' else c) bytes
 else if charset="UTF-8" then HostEncoding.utf8 bytes
 else try decode charset bytes with Invalid_argument _->invalid_arg "The character set provided in ContentType is invalid. Cannot read content as string using an invalid character set."
let get client url=let status,body,contentType=getBytes client url in status,decodeContent contentType body
