(* PackageIO.ml - SQLite response cache and OCaml HTTP/TLS transport. *)
[@@@warning "-4"]

let withCache path action =
  let db = Sqlite3.db_open path in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.db_close db))
    (fun () ->
      Sqlite3.busy_timeout db 5000;
      Sqlite3.exec db
        "CREATE TABLE IF NOT EXISTS package_responses (cache_key TEXT PRIMARY \
         KEY, status INTEGER NOT NULL, body TEXT NOT NULL)"
      |> Sqlite3.Rc.check;
      action db)

let withStatement db sql action =
  let statement = Sqlite3.prepare db sql in
  Fun.protect
    ~finally:(fun () -> ignore (Sqlite3.finalize statement))
    (fun () -> action statement)

let cacheRead path key =
  withCache path (fun db ->
      withStatement db
        "SELECT status, body FROM package_responses WHERE cache_key = ?"
        (fun statement ->
          Sqlite3.bind_text statement 1 key |> Sqlite3.Rc.check;
          match Sqlite3.step statement with
          | Sqlite3.Rc.DONE -> None
          | Sqlite3.Rc.ROW ->
              Some
                (Sqlite3.column_int statement 0, Sqlite3.column_text statement 1)
          | code ->
              raise
                (Sqlite3.SqliteError
                   (Sqlite3.Rc.to_string code ^ ": " ^ Sqlite3.errmsg db))))

let cacheWrite path key status body =
  withCache path (fun db ->
      withStatement db
        "INSERT INTO package_responses(cache_key,status,body) VALUES (?,?,?) \
         ON CONFLICT(cache_key) DO UPDATE SET status=excluded.status, \
         body=excluded.body" (fun statement ->
          Sqlite3.bind_text statement 1 key |> Sqlite3.Rc.check;
          Sqlite3.bind_int statement 2 status |> Sqlite3.Rc.check;
          Sqlite3.bind_text statement 3 body |> Sqlite3.Rc.check;
          Sqlite3.step statement |> Sqlite3.Rc.check))

type client = { mutable disposed : bool }

let create () = { disposed = false }
let dispose client = client.disposed <- true

(* Lwt's Unix scheduler is shared by callers in the threaded test runner. *)
let scheduler = Mutex.create ()

let get client url =
  Mutex.lock scheduler;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock scheduler)
    (fun () ->
      if client.disposed then invalid_arg "HTTP client has been disposed";
      Conduit_lwt_unix.tls_library := Conduit_lwt_unix.Native;
      let open Lwt.Syntax in
      let rec fetch remaining uri =
        (match Uri.scheme uri with
        | Some ("http" | "https") -> ()
        | _ -> invalid_arg "Package URL must use HTTP or HTTPS");
        let* ctx = Conduit_lwt_unix.init () in
        let ctx = Cohttp_lwt_unix.Client.custom_ctx ~ctx () in
        let* response, body = Cohttp_lwt_unix.Client.get ~ctx uri in
        let status =
          Cohttp.Response.status response |> Cohttp.Code.code_of_status
        in
        let headers = Cohttp.Response.headers response in
        let* bytes = Cohttp_lwt.Body.to_string body in
        match (status, Cohttp.Header.get headers "location") with
        | (301 | 302 | 303 | 307 | 308), Some location ->
            if remaining = 0 then
              Lwt.fail (Invalid_argument "Too many package HTTP redirects")
            else
              fetch (remaining - 1)
                (Uri.resolve "http" uri (Uri.of_string location))
        | _ ->
            Lwt.return
              ( status,
                ContentEncoding.decodeContent
                  (Cohttp.Header.get headers "content-type")
                  bytes )
      in
      Lwt_main.run
        (Lwt_unix.with_timeout 30. (fun () -> fetch 50 (Uri.of_string url))))
