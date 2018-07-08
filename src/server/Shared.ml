module type DataS = sig
  type t
end

module type S = sig
  type elt

  type t

  val init : elt -> t

  val apply : t -> f:(elt -> 'a) -> 'a

  val update : t -> f:(elt -> elt) -> unit

  val apply_update : t -> f:(elt -> 'a * elt) -> 'a
end

module Make (D : DataS) : S with type elt = D.t = struct
  type elt = D.t

  type t = {mutable data: elt; mutex: Mutex.t}

  let with_lock {data; mutex} ~f =
    Mutex.lock mutex ;
    let v = try f data with e -> Mutex.unlock mutex ; raise e in
    Mutex.unlock mutex ; v

  let init data = {data; mutex= Mutex.create ()}

  let apply x ~f = with_lock x ~f:(fun data -> f data)

  let update x ~f = apply x ~f:(fun data -> x.data <- f data)

  let apply_update x ~f =
    with_lock x ~f:(fun data ->
        let v, data' = f data in
        x.data <- data' ;
        v )
end
