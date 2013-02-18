open Yojson.Basic

exception Parse_error of string

let get_obj = function
  | `Assoc o -> o
  | _ -> raise (Parse_error "Not an object")

let get_string = function
  | `String s -> s
  | _ -> raise (Parse_error "Not a string")

let get_int = function
  | `Int i -> i
  | _ -> raise (Parse_error "Not an int")

let get_list = function
  | `List l -> l
  | _ -> raise (Parse_error "Not a list") 

let get_float = function
  | `Float f -> f
  | _ -> raise (Parse_error "Not a float")
