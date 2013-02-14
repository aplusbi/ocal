open Yojson.Safe
open Batteries
open BatPervasives

let get_result out str =
  out := !out ^ str;
  BatString.length str

type config = {
  client_id : string;
  client_secret : string;
}

type token = {
  access_token : string;
  token_type : string;
  expires_in : int;
  refresh_token : string;
}

let read_config cfg =
  let options = BatEnum.fold (fun l s -> let k, v = BatString.split s ":" in
      (BatString.trim k, BatString.trim v)::l) [] cfg in
  {client_id = BatList.assoc "client_id" options;
   client_secret = BatList.assoc "client_secret" options}

let write_token token =
    let count = ref 0 in
    let next () =
        count := !count + 1;
        match !count with
        | 1 -> "{"
        | 2 -> "\"access_token\" : \"" ^ token.access_token ^ "\","
        | 3 -> "\"token_type\" : \"" ^ token.token_type ^ "\","
        | 4 -> "\"expires_in\" : " ^ (string_of_int token.expires_in) ^ ","
        | 5 -> "\"refresh_token\" : \"" ^ token.refresh_token ^ "\""
        | 6 -> "}"
        | _ -> raise BatEnum.No_more_elements
    in
    BatFile.write_lines ".token" (BatEnum.from next)

let read_token = function
  | `Assoc l -> 
    begin
      let at = BatList.assoc "access_token" l in
      let tt = BatList.assoc "token_type" l in
      let ei = BatList.assoc "expires_in" l in
      let rt = BatList.assoc "refresh_token" l in
      match (at, tt, ei, rt) with
      | `String at', `String tt', `Int ei', `String rt' -> 
        let newtk = { access_token = at';
                      token_type = tt';
                      expires_in = ei';
                      refresh_token = rt' }
        in
        newtk
      | _ -> raise Not_found
    end
  | _ -> raise Not_found

let read_refresh_token tkn = function
  | `Assoc l -> 
    begin
      let at = BatList.assoc "access_token" l in
      let tt = BatList.assoc "token_type" l in
      let ei = BatList.assoc "expires_in" l in
      match (at, tt, ei) with
      | `String at', `String tt', `Int ei' ->
        let newtk = { access_token = at';
                      token_type = tt';
                      expires_in = ei';
                      refresh_token = tkn.refresh_token }
        in
        write_token newtk;
        newtk
      | _ -> raise Not_found
    end
  | _ -> raise Not_found


let get_oauth cfg = 
  let baseurl = "https://accounts.google.com/o/oauth2/auth?" in
  let params = [("scope", Http.url_encode "https://www.googleapis.com/auth/calendar.readonly");
                ("response_type", "code");
                ("client_id", cfg.client_id);
                ("redirect_uri", "urn:ietf:wg:oauth:2.0:oob");
                ("state", "ocal")]
  in
  let encoded_params = BatList.map (fun (k, v) -> k ^ "=" ^ v) params
  in
  let url = baseurl ^ (BatString.join "\\&" encoded_params) in
  ignore (BatUnix.system ("firefox " ^ url));
  read_line ()

let get_token cfg auth =
  let content = [("code", auth);
                 ("client_id", cfg.client_id);
                 ("client_secret", cfg.client_secret);
                 ("redirect_uri", "urn:ietf:wg:oauth:2.0:oob");
                 ("grant_type", "authorization_code")]
  in
  let url = "https://accounts.google.com/o/oauth2/token" in
  match Http.post ~content:content url with
  | (200, _, s) -> s
  | (s, _, _) -> raise Not_found

let refresh_token cfg tkn =
  let content = [("refresh_token", tkn.refresh_token);
                 ("client_id", cfg.client_id);
                 ("client_secret", cfg.client_secret);
                 ("grant_type", "refresh_token")]
  in
  let url = "https://accounts.google.com/o/oauth2/token" in
  match Http.post ~content:content url with
  | (200, _, s) -> s
  | (s, _, _) -> print_int s; print_newline (); raise Not_found


let create_auth_header tkn =
  "Authorization: Bearer " ^ tkn.access_token

let calendar_list auth =
  let header = [auth] in
  let url = "https://www.googleapis.com/calendar/v3/users/me/calendarList" in
  Http.get ~header:header url

let event_list auth cal_id =
  let header = [auth] in
  let params = [("timeMin", "2013-02-10T00:00:00-04:00");
                ("timeMax", "2013-03-28T00:00:00-04:00");
                ("maxResults", "2")]
  in
  let url = "https://www.googleapis.com/calendar/v3/calendars/" ^ cal_id ^ "/events" in
  Http.get ~header:header ~params:params url

let () =
  try
    Curl.global_init Curl.CURLINIT_GLOBALSSL;

    let cfg = BatFile.lines_of ".config" |> read_config in
    let token =
      try
        from_file ".token" |> read_token
      with Not_found | Sys_error(_) ->
        print_endline "Not found";
        let auth = get_oauth cfg in
        get_token cfg auth |> from_string |> read_token
    in
    let auth = create_auth_header token in

    let rec loop a =
      match event_list a "aplusbi@gmail.com" with
      | (200, _, s) ->
        let j = from_string s in
        j |> pretty_to_string |> print_endline;
        BatUnix.sleep 5;
        loop a
      | (401, _, _) -> let t = refresh_token cfg token in
        let tkn = from_string t |> read_refresh_token token in
        loop (create_auth_header tkn)
      | (s, _, _) -> print_int s; print_newline ()
    in
    loop auth;

    Curl.global_cleanup ()
  with Curl.CurlException(code, status, msg) ->
    print_endline (Curl.strerror code);
    print_int status;
    print_newline ();
    print_endline msg; 
    Curl.global_cleanup ()
