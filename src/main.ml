open Yojson.Basic
open Batteries
open BatPervasives
open Config

let create_auth_header tkn =
  let {Auth.access_token = access_token} = tkn in
  "Authorization: Bearer " ^ access_token

let print_event e =
  let { Cal.summary = summary } = e in
  print_endline summary

let () =
  try
    Curl.global_init Curl.CURLINIT_GLOBALSSL;

    let cfg = BatFile.lines_of ".config" |> read_config in
    let token =
      try
        from_file ".token" |> Auth.read_token
      with Not_found | Sys_error(_) ->
        print_endline "Not found";
        let auth = Auth.get_oauth cfg in
        Auth.get_token cfg auth |> from_string |> Auth.read_token
    in
    let auth = create_auth_header token in

    let rec loop a =
      match Cal.event_list a "aplusbi@gmail.com" with
      | (200, l) ->
        BatList.iter print_event l;
        BatUnix.sleep 5;
        loop a
      | (401, _) -> let t = Auth.refresh_token cfg token in
        let tkn = from_string t |> Auth.read_refresh_token token in
        loop (create_auth_header tkn)
      | (s, _) -> print_int s; print_newline ()
    in
    loop auth;

    Curl.global_cleanup ()
  with Curl.CurlException(code, status, msg) ->
    print_endline (Curl.strerror code);
    print_int status;
    print_newline ();
    print_endline msg; 
    Curl.global_cleanup ()
