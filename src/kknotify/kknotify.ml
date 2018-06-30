let key = Config.get_key ()

let main () =
  if Array.length Sys.argv < 2 then (
    Format.eprintf "ERROR: need an argument@." ;
    exit 1 ) ;
  let msg =
    List.init (Array.length Sys.argv - 1) (fun i -> Sys.argv.(i + 1))
    |> String.concat " "
  in
  let address = ((Unix.gethostbyname Config.host).Unix.h_addr_list).(0) in
  let sockaddr = Unix.ADDR_INET (address, Config.port) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let ic, oc =
    try
      Unix.connect sock sockaddr ;
      (Unix.in_channel_of_descr sock, Unix.out_channel_of_descr sock)
    with exn ->
      Unix.close sock ;
      Format.eprintf "ERROR: failed to connect kknotify server@." ;
      raise exn
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
