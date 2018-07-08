module Id = struct
  type t = int

  let compare = compare

  let pp fmt x = Format.fprintf fmt "%d" x
end

(* authorized ids *)
module AuthIds : sig
  val mem : Id.t -> bool

  val add : Id.t -> unit

  val remove : Id.t -> unit
end = struct
  module M = SharedSet.Make (Id)

  let auth_ids = M.empty ()

  let lift ~f id = f id auth_ids

  let mem = lift ~f:M.mem

  let add = lift ~f:M.add

  let remove id =
    M.remove id auth_ids ;
    Format.eprintf "current auth_ids: %a@." M.pp auth_ids
end

module Msg = struct
  type t = string
end

(* message queue *)
module MsgQueue : sig
  val pop_msg : unit -> Msg.t option

  val process_msg : Id.t -> Msg.t -> unit
end = struct
  module M = SharedQueue.Make (Msg)

  let msgs = M.empty ()

  let pop_msg () =
    match M.pop msgs with msg -> Some msg | exception M.Empty -> None

  let process_msg =
    let key = Config.get_key () in
    fun id msg ->
      if msg = key then (
        Format.eprintf "thread %d is authorized@." id ;
        AuthIds.add id )
      else if AuthIds.mem id then (
        Format.eprintf "add msg from %d to queue@." id ;
        M.push msg msgs )
      else Format.eprintf "ignore msg from %d to queue@." id
end

(* map from id to output channel *)
module OutChans : sig
  val add : Id.t -> out_channel -> unit

  val remove : Id.t -> unit

  val bindings : unit -> (Id.t * out_channel) list
end = struct
  module M =
    SharedMap.Make (Id)
      (struct
        type t = out_channel
      end)

  let ocs = M.empty ()

  let add id oc =
    Format.eprintf "start listen thread: %d@." id ;
    M.add id oc ocs

  let remove id =
    Format.eprintf "stop listen thread: %d@." id ;
    M.remove id ocs ;
    Format.eprintf "currently listening: %a@." M.pp ocs

  let bindings () = M.bindings ocs
end

let remove_id id = OutChans.remove id ; AuthIds.remove id

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

let listen_thread fd =
  let strip_CR s =
    let len = String.length s in
    if int_of_char s.[len - 1] = 13 then String.sub s 0 (len - 1) else s
  in
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

let main () =
  let ignore_thread (_: Thread.t) = () in
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
  Format.eprintf "stop kknotify server@."

let () = main ()
