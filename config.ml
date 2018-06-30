(* START USER OPTION *)
let host = "kkeun.net"

let port = 20004

let key_path = ".kknotify.key"

(* END USER OPTION *)

let get_key () =
  let ic =
    try open_in key_path with Sys_error _ ->
      Format.eprintf "ERROR: run 'kknotify-key-gen' first@." ;
      exit 1
  in
  try
    let v = input_line ic in
    close_in ic ; v
  with e -> close_in_noerr ic ; raise e
