module type DataS = sig
  type t
end

module type S = sig
  type elt

  type t

  val init : elt -> t

  val apply : t -> f:(elt -> 'a) -> 'a

  val update : t -> f:(elt -> elt) -> unit
end

module Make (D : DataS) : S with type elt = D.t = struct
  type elt = D.t

  type t = {mutable data: elt; mutex: Mutex.t}

  let with_lock {data; mutex} ~f =
    Mutex.lock mutex ;
    let v = f data in
    Mutex.unlock mutex ; v

  let init data = {data; mutex= Mutex.create ()}

  let apply x ~f = with_lock x ~f:(fun data -> f data)

  let update x ~f = apply x ~f:(fun data -> x.data <- f data)
end
