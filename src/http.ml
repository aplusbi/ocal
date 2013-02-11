open Batteries

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

let post url content ?header =
    let c = postfields_of_content content in
    let result = ref "" in
    let conn = Curl.init () in
    Curl.set_url conn url;
    Curl.set_httppost conn c;
    Curl.set_writefunction conn (get_result result);
    Curl.perform conn;
    Curl.cleanup conn;
    !result
