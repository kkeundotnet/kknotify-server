module Id = struct
  type t = int

  let compare = compare
end

module IdSet = Set.Make (Id)
module IdMap = Map.Make (Id)

let with_lock mutex ~f =
  Mutex.lock mutex ;
  let v = f () in
  Mutex.unlock mutex ; v

module MsgQueue = struct
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

  let add_msg =
    let key = Config.get_key () in
    fun id msg ->
      if msg = key then (
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

let braodcast_thread () =
  let send_msg msg =
    let f (id, oc) =
      try output_string oc msg ; output_char oc '\n' ; flush oc
      with Sys_error _ -> ()
    in
    List.iter f (OutChans.bindings ())
  in
  Format.eprintf "start broadcast thread@." ;
  while true do
    (* TODO: Polling is not cool. :( *)
    Unix.sleep 2 ;
    match MsgQueue.pop_msg () with None -> () | Some msg -> send_msg msg
  done

let strip_CR s =
  let len = String.length s in
  if int_of_char s.[len - 1] = 13 then String.sub s 0 (len - 1) else s

let listen_thread fd =
  let exception StopListen in
  let id = Thread.id (Thread.self ()) in
  let ic, oc = (Unix.in_channel_of_descr fd, Unix.out_channel_of_descr fd) in
  OutChans.add id oc ;
  Format.eprintf "start listen thread: %d@." id ;
  try
    while true do
      let msg = try strip_CR (input_line ic) with _ -> raise StopListen in
      MsgQueue.add_msg id msg
    done
  with StopListen ->
    OutChans.remove id ;
    Format.eprintf "stop listen thread: %d@." id

let ignore_thread (_: Thread.t) = ()

let main () =
  Format.eprintf "start kknotify server@." ;
  ignore_thread (Thread.create braodcast_thread ()) ;
  let address = ((Unix.gethostbyname Config.host).Unix.h_addr_list).(0) in
  let sockaddr = Unix.ADDR_INET (address, Config.port) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind sock sockaddr ;
  Unix.listen sock 3 ;
  while true do
    let fd, caller = Unix.accept sock in
    ignore_thread (Thread.create listen_thread fd)
  done ;
  prerr_endline "stop kknotify server"

let () = main ()
