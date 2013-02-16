open Yojson.Safe
open Batteries
open Config

type token = {
  access_token : string;
  token_type : string;
  expires_in : int;
  refresh_token : string;
}

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


