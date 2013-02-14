open Batteries
open Pervasives

let url_encode s =
  let encode = function
    | '/' -> "%2F"
    | ':' -> "%3A"
    | '?' -> "%3F"
    | '&' -> "%26"
    | '@' -> "%40"
    | c -> BatString.of_char c
  in
  BatString.replace_chars encode s

let get_result out str =
  out := !out ^ str;
  BatString.length str

let postfields_of_content content =
  BatList.map (fun (k, v) -> Curl.CURLFORM_CONTENT (k, v, Curl.DEFAULT))
    content

let post ?header ?content url =
  let result = ref "" in
  let result_header = ref "" in
  let conn = Curl.init () in

  Curl.set_url conn url;
  let h = match header with
    | Some l -> l
    | None -> []
  in
  Curl.set_httpheader conn h;
  let c = match content with
    | Some l -> postfields_of_content l
    | None -> []
  in
  Curl.set_httppost conn c;

  Curl.set_writefunction conn (get_result result);
  Curl.set_headerfunction conn (get_result result_header);

  Curl.perform conn;

  let status = Curl.get_httpcode conn in
  Curl.cleanup conn;
  (status, !result_header, !result)

let get ?header ?params url =
  let result = ref "" in
  let result_header = ref "" in
  let conn = Curl.init () in

  let h = match header with
    | Some l -> l
    | None -> []
  in
  Curl.set_httpheader conn h;
  let url = match params with
    | Some l -> url ^
        "?" ^
        (BatList.map (fun (k, v) -> k ^ "=" ^ v) l |> BatString.join "&")
    | None -> url
  in
  Curl.set_url conn url;

  Curl.set_writefunction conn (get_result result);
  Curl.set_headerfunction conn (get_result result_header);

  Curl.perform conn;

  let status = Curl.get_httpcode conn in
  Curl.cleanup conn;
  (status, !result_header, !result)
