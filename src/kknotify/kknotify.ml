let address = ((Unix.gethostbyname (Unix.gethostname ())).Unix.h_addr_list).(0)

(* TODO: change default port & optional port number *)
let port = 10004

let key =
  let ic =
    try open_in ".key" with Sys_error _ ->
      Format.eprintf "ERROR: run 'make key_gen'@." ;
      exit 1
  in
  try
    let v = input_line ic in
    close_in ic ; v
  with e -> close_in_noerr ic ; raise e

let main () =
  if Array.length Sys.argv < 2 then (
    Format.eprintf "ERROR: need an argument@." ;
    exit 1 ) ;
  let msg =
    List.init (Array.length Sys.argv - 1) (fun i -> Sys.argv.(i + 1))
    |> String.concat " "
  in
  let sockaddr = Unix.ADDR_INET (address, port) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let ic, oc =
    try
      Unix.connect sock sockaddr ;
      (Unix.in_channel_of_descr sock, Unix.out_channel_of_descr sock)
    with exn -> Unix.close sock ; raise exn
  in
  ( try
      output_string oc key ;
      output_char oc '\n' ;
      output_string oc msg ;
      output_char oc '\n' ;
      flush oc
    with Sys_error _ -> () ) ;
  Unix.shutdown sock Unix.SHUTDOWN_SEND

let () = main ()
