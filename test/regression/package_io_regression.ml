(* package_io_regression.ml - Durable cache and Unicode transport boundaries. *)
open Dark_compiler

let require condition message = if not condition then failwith message

let () =
  let path = Filename.temp_file "package-cache-" ".sqlite3" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let db = Sqlite3.db_open path in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          Sqlite3.exec db
            "CREATE TABLE package_responses (cache_key TEXT PRIMARY KEY, \
             status INTEGER NOT NULL, body TEXT NOT NULL); INSERT INTO \
             package_responses VALUES ('legacy',200,'existing response')"
          |> Sqlite3.Rc.check);
      require
        (PackageIO.cacheRead path "legacy" = Some (200, "existing response"))
        "Read existing SQLite schema";
      require (PackageIO.cacheRead path "missing" = None) "Missing cache entry";
      PackageIO.cacheWrite path "../key\000" 200 "body\000λ";
      require
        (PackageIO.cacheRead path "../key\000" = Some (200, "body\000λ"))
        "Cache round trip";
      PackageIO.cacheWrite path "../key\000" 404 "";
      require
        (PackageIO.cacheRead path "../key\000" = Some (404, ""))
        "Cache replacement";
      require
        (PackageIO.cacheRead path "../key" = None)
        "Cache keys preserve embedded NUL");
  List.iter
    (fun (charset, bytes, expected) ->
      require
        (ContentEncoding.decodeContent
           (Some ("text/plain; charset=" ^ charset))
           bytes
        = expected)
        ("Decode " ^ charset))
    [
      ("utf-16le", "\255\254A\000\061\216\000\222", "A😀");
      ("utf-16be", "\254\255\000A\216\061\222\000", "A😀");
      ("utf-32le", "\255\254\000\000A\000\000\000\000\246\001\000", "A😀");
      ("utf-32be", "\000\000\254\255\000\000\000A\000\001\246\000", "A😀");
      ("latin1", "\233", "é");
      ("utf-16le", "\000\216", "�");
      ("utf-32le", "\000\216\000\000x", "��");
    ];
  print_endline "Package cache and content encoding regressions passed"
