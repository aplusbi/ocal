open Yojson.Safe
open Batteries
open BatPervasives

let post_field k v =
    Curl.CURLFORM_CONTENT (k, v, Curl.DEFAULT)

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
    {client_id = BatList.assoc "client_id" options; client_secret = BatList.assoc
    "client_secret" options}

let read_token = function
    | `Assoc l -> 
            begin
                let at = BatList.assoc "access_token" l in
                let tt = BatList.assoc "token_type" l in
                let ei = BatList.assoc "expires_in" l in
                let rt = BatList.assoc "refresh_token" l in
                match (at, tt, ei, rt) with
                | `String at', `String tt', `Int ei', `String rt' -> 
                        { access_token = at'; token_type = tt'; expires_in = ei';
                        refresh_token = rt' }
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
                        { access_token = at'; token_type = tt'; expires_in = ei';
                        refresh_token = tkn.refresh_token }
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
    let content = [post_field "code" auth;
        post_field "client_id" cfg.client_id;
        post_field "client_secret" cfg.client_secret;
        post_field "redirect_uri" "urn:ietf:wg:oauth:2.0:oob";
        post_field "grant_type" "authorization_code"]
    in
    let result = ref "" in
    let conn = Curl.init () in
    Curl.set_url conn "https://accounts.google.com/o/oauth2/token";
    Curl.set_httppost conn content;
    Curl.set_writefunction conn (get_result result);
    Curl.perform conn;
    Curl.cleanup conn;
    !result

let refresh_token cfg tkn =
    let content = [post_field "refresh_token" tkn.refresh_token;
        post_field "client_id" cfg.client_id;
        post_field "client_secret" cfg.client_secret;
        post_field "grant_type" "refresh_token"]
    in
    let result = ref "" in
    let conn = Curl.init () in
    Curl.set_url conn "https://accounts.google.com/o/oauth2/token";
    Curl.set_httppost conn content;
    Curl.set_writefunction conn (get_result result);
    Curl.perform conn;
    Curl.cleanup conn;
    !result


let create_auth_header tkn =
    "Authorization: Bearer " ^ tkn.access_token

let calendar_list auth =
    let header = [auth] in
    let result = ref "" in
    let conn = Curl.init () in
    Curl.set_url conn
    "https://www.googleapis.com/calendar/v3/users/me/calendarList"; Curl.set_httpheader conn header;
    Curl.set_writefunction conn (get_result result);
    Curl.perform conn;
    Curl.cleanup conn;
    !result

let event_list auth cal_id =
    let header = [auth] in
    let result = ref "" in
    let conn = Curl.init () in
    let starttime = "2013-02-10T00:00:00-04:00" in
    let endtime = "2013-03-28T00:00:00-04:00" in
    Curl.set_url conn
    ("https://www.googleapis.com/calendar/v3/calendars/" ^ cal_id ^
    "/events?maxResults=2" ^ "&timeMin=" ^ starttime ^ "&timeMax=" ^ endtime); Curl.set_httpheader conn header;
    Curl.set_writefunction conn (get_result result);
    Curl.perform conn;
    Curl.cleanup conn;
    !result

let () =
    try
        Curl.global_init Curl.CURLINIT_GLOBALSSL;

        let cfg = BatFile.lines_of ".config" |> read_config in
        let token =
            try
                from_file ".token" |> read_token
            with Not_found | Sys_error(_) ->
                    let auth = get_oauth cfg in
                    get_token cfg auth |> from_string |> read_token
        in
        let auth = create_auth_header token in
        event_list auth "aplusbi@gmail.com" |> print_endline;
        Curl.global_cleanup ()
    with Curl.CurlException(code, status, msg) ->
        print_endline (Curl.strerror code);
        print_int status;
        print_newline ();
        print_endline msg; 
        Curl.global_cleanup ()
