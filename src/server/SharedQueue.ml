module type EltS = sig
  type t
end

module type S = sig
  type elt

  type t

  exception Empty

  val empty : unit -> t

  val add : elt -> t -> unit

  val push : elt -> t -> unit

  val take : t -> elt

  val pop : t -> elt

  val peek : t -> elt

  val top : t -> elt

  val is_empty : t -> bool

  val length : t -> int

  val iter : t -> f:(elt -> unit) -> unit

  val fold : t -> init:'a -> f:(elt -> 'a -> 'a) -> 'a
end

module Make (Elt : EltS) : S with type elt = Elt.t = struct
  module M = struct
    type t = Elt.t KKueue.t
  end

  module SM = Shared.Make (M)

  type elt = Elt.t

  type t = SM.t

  let lift_apply x ~f = SM.apply x ~f

  let lift_apply_update x ~f = SM.apply_update x ~f

  let lift_update_1 arg x ~f = SM.update x ~f:(f arg)

  exception Empty = KKueue.Empty

  let empty () = SM.init KKueue.empty

  let add = lift_update_1 ~f:KKueue.add

  let push = add

  let take = lift_apply_update ~f:KKueue.take

  let pop = take

  let peek = lift_apply ~f:KKueue.peek

  let top = peek

  let is_empty = lift_apply ~f:KKueue.is_empty

  let length = lift_apply ~f:KKueue.length

  let iter x ~f = SM.apply x ~f:(KKueue.iter ~f)

  let fold x ~init ~f = SM.apply x ~f:(KKueue.fold ~init ~f)
end
