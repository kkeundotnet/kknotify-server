module type EltS = sig
  type t
end

module type NormalS = sig
  type elt

  type t

  exception Empty

  val empty : t

  val add : elt -> t -> t

  val push : elt -> t -> t

  val take : t -> elt * t

  val pop : t -> elt * t

  val peek : t -> elt

  val top : t -> elt

  val is_empty : t -> bool

  val length : t -> int

  val iter : t -> f:(elt -> unit) -> unit

  val fold : t -> init:'a -> f:(elt -> 'a -> 'a) -> 'a
end

module Normal (Elt : EltS) : NormalS with type elt = Elt.t = struct
  type elt = Elt.t

  type t = {old_stack: elt list; new_stack: elt list}

  exception Empty

  let empty = {old_stack= []; new_stack= []}

  let add e x = {x with new_stack= e :: x.new_stack}

  let push = add

  let take x =
    match x.new_stack with
    | [] -> (
      match List.rev x.old_stack with
      | [] -> raise Empty
      | e :: tl -> (e, {new_stack= tl; old_stack= []}) )
    | e :: tl -> (e, {x with new_stack= tl})

  let pop = take

  let peek x =
    let e, _x = take x in
    e

  let top = peek

  let is_empty {old_stack; new_stack} =
    match (old_stack, new_stack) with [], [] -> true | _, _ -> false

  let length {old_stack; new_stack} =
    List.length old_stack + List.length new_stack

  let iter {old_stack; new_stack} ~f =
    List.iter f old_stack ;
    List.iter f (List.rev new_stack)

  let fold {old_stack; new_stack} ~init ~f =
    let rec list_fold x ~init ~f =
      match x with [] -> init | hd :: tl -> list_fold tl ~init:(f hd init) ~f
    in
    let acc = list_fold old_stack ~init ~f in
    list_fold (List.rev new_stack) ~init:acc ~f
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
  module M = Normal (Elt)
  module SM = Shared.Make (M)

  type elt = Elt.t

  type t = SM.t

  let lift_apply x ~f = SM.apply x ~f

  let lift_apply_update x ~f = SM.apply_update x ~f

  let lift_update_1 arg x ~f = SM.update x ~f:(f arg)

  exception Empty = M.Empty

  let empty () = SM.init M.empty

  let add = lift_update_1 ~f:M.add

  let push = add

  let take = lift_apply_update ~f:M.take

  let pop = take

  let peek = lift_apply ~f:M.peek

  let top = peek

  let is_empty = lift_apply ~f:M.is_empty

  let length = lift_apply ~f:M.length

  let iter x ~f = SM.apply x ~f:(M.iter ~f)

  let fold x ~init ~f = SM.apply x ~f:(M.fold ~init ~f)
end
