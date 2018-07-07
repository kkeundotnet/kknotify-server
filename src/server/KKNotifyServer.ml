module Id = struct
  type t = int

  let compare = compare
end

module MsgQueue = struct
  module SharedIdSet = SharedSet.Make (Id)

  let authorized_ids = SharedIdSet.empty ()

  let is_authorized id = SharedIdSet.mem id authorized_ids

  let add_authorized_id id = SharedIdSet.add id authorized_ids

  let remove_authorized_id id = SharedIdSet.remove id authorized_ids

  module SharedMsg = Shared.Make (struct
    type t = string Queue.t
  end)

  let msgs = SharedMsg.init (Queue.create ())

  let pop_msg () =
    SharedMsg.apply msgs ~f:(fun msgs ->
        match Queue.pop msgs with
        | msg -> Some msg
        | exception Queue.Empty -> None )

  let push_msg msg = SharedMsg.apply msgs ~f:(Queue.push msg)

  let process_msg =
    let key = Config.get_key () in
    fun id msg ->
      if msg = key then (
        Format.eprintf "thread %d is authorized@." id ;
        add_authorized_id id )
      else if is_authorized id then (
        Format.eprintf "add msg from %d to queue@." id ;
        push_msg msg )
      else Format.eprintf "ignore msg from %d to queue@." id
end

module OutChans = struct
  module IdMap = Map.Make (Id)

  module SharedOutChans = Shared.Make (struct
    type t = out_channel IdMap.t
  end)

  let ocs = SharedOutChans.init IdMap.empty

  let add id oc =
    Format.eprintf "start listen thread: %d@." id ;
    SharedOutChans.update ocs ~f:(IdMap.add id oc)

  let remove id =
    Format.eprintf "stop listen thread: %d@." id ;
    SharedOutChans.update ocs ~f:(IdMap.remove id)

  let bindings () = SharedOutChans.apply ocs ~f:IdMap.bindings
end

let remove_id id =
  OutChans.remove id ;
  MsgQueue.remove_authorized_id id

let braodcast_thread () =
  let send_msg msg =
    let f (id, oc) =
      try output_string oc msg ; output_char oc '\n' ; flush oc
      with Sys_error _ -> remove_id id
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
  try
    while true do
      let msg = try strip_CR (input_line ic) with _ -> raise StopListen in
      MsgQueue.process_msg id msg
    done
  with StopListen -> remove_id id

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
