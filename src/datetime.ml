open Batteries
open BatPervasives
open BatUnix
open Utils

let parse_zone z =
  let h, m = BatString.split z ":" in
  int_of_string h

let parse s =
  try
    let date, time = BatString.split s "T" in
    let ymd = BatString.nsplit date "-"
              |> BatList.map int_of_string
              |> Array.of_list in
    let time, zone =
      try
        let t, z = BatString.split time "-" in
        (t, -(parse_zone z))
      with Not_found ->
        let t, z = BatString.split time "+" in
        (t, parse_zone z)
    in
    let hms = BatString.nsplit time ":"
              |> BatList.map int_of_string
              |> Array.of_list in
    let tm =
      {
        tm_hour = hms.(0) + zone;
        tm_min = hms.(1);
        tm_sec = hms.(2);
        tm_year = ymd.(0);
        tm_mon = ymd.(1) - 1;
        tm_mday = ymd.(0);
        tm_wday = 0;
        tm_yday = 0;
        tm_isdst = false
      }
    in
    let _, tm = mktime tm in
    tm
  with
  | Not_found
  | Invalid_argument(_) -> raise (Parse_error "Error parsing datetime")
