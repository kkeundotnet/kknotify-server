let address = ((Unix.gethostbyname (Unix.gethostname ())).Unix.h_addr_list).(0)

let port = 10004

let with_lock mutex ~f =
  Mutex.lock mutex ;
  let v = f () in
  Mutex.unlock mutex ; v

module Id = struct
  type t = int

  let compare = compare
end

module IdSet = Set.Make (Id)
module IdMap = Map.Make (Id)

module MsgQueue = struct
  let is_authorization_password msg =
    Sha512.to_hex (Sha512.string msg)
    = "db672c0fcfbc6c2f5b889d7a8e42c67260348d5ac8d47c7ecd1b3d50265ce4e31a32c6f8662528d6e01f041cf7a79c9bd35038d5166629e94b1c2432be872894"

  let authorized_ids, authorized_ids_mutex = (ref IdSet.empty, Mutex.create ())

  let is_authorized id =
    with_lock authorized_ids_mutex ~f:(fun () -> IdSet.mem id !authorized_ids)

  let add_authorized_id id =
    with_lock authorized_ids_mutex ~f:(fun () ->
        authorized_ids := IdSet.add id !authorized_ids )

  let msgs, msgs_mutex = ((Queue.create () : string Queue.t), Mutex.create ())

  let pop_msg () =
    with_lock msgs_mutex ~f:(fun () ->
        match Queue.pop msgs with
        | msg -> Some msg
        | exception Queue.Empty -> None )

  let add_msg id msg =
    if is_authorization_password msg then (
      Format.eprintf "thread %d is authorized@." id ;
      add_authorized_id id )
    else if is_authorized id then (
      Format.eprintf "add msg from %d to queue@." id ;
      with_lock msgs_mutex ~f:(fun () -> Queue.add msg msgs) )
    else Format.eprintf "ignore msg from %d to queue@." id
end

module OutChans = struct
  let ocs, ocs_mutex = (ref IdMap.empty, Mutex.create ())

  let add id oc = with_lock ocs_mutex ~f:(fun () -> ocs := IdMap.add id oc !ocs)

  let remove id = with_lock ocs_mutex ~f:(fun () -> ocs := IdMap.remove id !ocs)

  let bindings () = with_lock ocs_mutex ~f:(fun () -> IdMap.bindings !ocs)
end

let start_braodcast_thread () =
  let send_msg msg =
    let msg_endline = msg ^ "\n" in
    let f (id, oc) =
      try
        output_string oc msg_endline ;
        flush oc
      with Sys_error _ -> ()
    in
    List.iter f (OutChans.bindings ())
  in
  let f () =
    Format.eprintf "start broadcast thread@." ;
    while true do
      (* TODO: Polling is not cool. :( *)
      Unix.sleep 2 ;
      match MsgQueue.pop_msg () with None -> () | Some msg -> send_msg msg
    done
  in
  Thread.create f ()

let start_listen_thread fd =
  let id = Thread.id (Thread.self ()) in
  let ic, oc = (Unix.in_channel_of_descr fd, Unix.out_channel_of_descr fd) in
  OutChans.add id oc ;
  Format.eprintf "start listen thread: %d@." id ;
  try while true do MsgQueue.add_msg id (input_line ic) done with _ ->
    OutChans.remove id ;
    Format.eprintf "stop listen thread: %d@." id

let main () =
  Format.eprintf "start kknotify server@." ;
  ignore (start_braodcast_thread () : Thread.t) ;
  let sockaddr = Unix.ADDR_INET (address, port) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind sock sockaddr ;
  Unix.listen sock 3 ;
  while true do
    let fd, caller = Unix.accept sock in
    ignore (Thread.create start_listen_thread fd : Thread.t)
  done ;
  prerr_endline "stop kknotify server"

let () = main ()
