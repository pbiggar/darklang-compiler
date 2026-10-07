(* package_http_regression.ml - HTTP redirects, chunking and TLS authentication. *)
open Dark_compiler
let require condition message = if not condition then failwith message
let certificate directory =
 let privateKey = X509.Private_key.generate ~bits:2048 `RSA in
 let subject = X509.Distinguished_name.[Relative_distinguished_name.singleton (CN (Common_name.v "localhost"))] in
 let request = match X509.Signing_request.create subject privateKey with
  | Ok request -> request | Error (`Msg message) -> failwith message in
 let time seconds = match Ptime.of_float_s seconds with
  | Some time -> time | None -> failwith "Invalid certificate test time" in
 let now = Unix.gettimeofday () in
 let names = X509.General_name.add X509.General_name.DNS ["localhost"] X509.General_name.empty in
 let extensions = X509.Extension.(empty
  |> add Basic_constraints (true,(true,None))
  |> add Key_usage (true,[`Digital_signature;`Key_encipherment;`Key_cert_sign])
  |> add Subject_alt_name (false,names)) in
 let certificate = match X509.Signing_request.sign request ~valid_from:(time (now -. 3600.))
   ~valid_until:(time (now +. 86400.)) ~extensions privateKey subject with
  | Ok certificate -> certificate
  | Error error -> failwith (Format.asprintf "%a" X509.Validation.pp_signature_error error) in
 List.iter (fun (name,contents) -> Out_channel.with_open_bin (Filename.concat directory name)
  (fun channel -> Out_channel.output_string channel contents))
  ["cert.pem",X509.Certificate.encode_pem certificate;"key.pem",X509.Private_key.encode_pem privateKey]
let () =
 match Array.to_list Sys.argv with
 | [_;"--certificate";directory] -> certificate directory
 | [_;http;https;mode] ->
  let client = PackageIO.create () in
  Fun.protect ~finally:(fun () -> PackageIO.dispose client) (fun () ->
   require (PackageIO.get client (http ^ "/redirect") = (200,"A😀")) "Redirect or UTF-16 response decoding";
   require (PackageIO.get client (http ^ "/missing") = (404,"missing")) "HTTP status preservation";
   require (PackageIO.get client (http ^ "/chunked") = (200,String.make 100000 'x')) "Chunked response body";
   if mode = "trusted" then require (PackageIO.get client https = (200,"secure")) "Verified TLS response"
   else (
    let rejected = try ignore (PackageIO.get client https); false with
     | Tls_lwt.Tls_failure _ -> true in
    require rejected "Untrusted TLS certificate accepted"));
  PackageIO.dispose client;
  let rejected = try ignore (PackageIO.get client http); false with Invalid_argument _ -> true in
  require rejected "Disposed HTTP client accepted a request";
  print_endline ("OCaml HTTP/TLS regressions passed (" ^ mode ^ ")")
 | _ -> failwith "Expected HTTP URL, HTTPS URL and TLS mode"
