open Batteries

type config = {
  client_id : string;
  client_secret : string;
}

let read_config cfg =
  let options = BatEnum.fold (fun l s -> let k, v = BatString.split s ":" in
      (BatString.trim k, BatString.trim v)::l) [] cfg in
  {client_id = BatList.assoc "client_id" options;
   client_secret = BatList.assoc "client_secret" options}


