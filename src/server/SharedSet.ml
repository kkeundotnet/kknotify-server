module type S = sig
  type elt

  type t

  val empty : unit -> t

  val singleton : elt -> t

  val of_list : elt list -> t

  val is_empty : t -> bool

  val mem : elt -> t -> bool

  val add : elt -> t -> unit

  val remove : elt -> t -> unit

  val iter : t -> f:(elt -> unit) -> unit

  val map : t -> f:(elt -> elt) -> unit

  val fold : t -> init:'a -> f:(elt -> 'a -> 'a) -> 'a

  val for_all : t -> f:(elt -> bool) -> bool

  val exists : t -> f:(elt -> bool) -> bool

  val filter : t -> f:(elt -> bool) -> unit

  val cardinal : t -> int

  val elements : t -> elt list

  val min_elt : t -> elt

  val min_elt_opt : t -> elt option

  val max_elt : t -> elt

  val max_elt_opt : t -> elt option

  val choose : t -> elt

  val choose_opt : t -> elt option

  val find : elt -> t -> elt

  val find_opt : elt -> t -> elt option

  val find_first : t -> f:(elt -> bool) -> elt

  val find_first_opt : t -> f:(elt -> bool) -> elt option

  val find_last : t -> f:(elt -> bool) -> elt

  val find_last_opt : t -> f:(elt -> bool) -> elt option
end

module Make (Ord : Set.OrderedType) : S with type elt = Ord.t = struct
  module M = Set.Make (Ord)
  module SM = Shared.Make (M)

  type elt = Ord.t

  type t = SM.t

  let lift_apply x ~f = SM.apply x ~f

  let lift_apply_1 arg x ~f = SM.apply x ~f:(f arg)

  let lift_update_1 arg x ~f = SM.update x ~f:(f arg)

  let lift_apply_g ~g x ~f = SM.apply x ~f:(g f)

  let lift_update_g ~g x ~f = SM.update x ~f:(g f)

  let empty () = SM.init M.empty

  let singleton e = SM.init (M.singleton e)

  let of_list l = SM.init (M.of_list l)

  let is_empty = lift_apply ~f:M.is_empty

  let mem = lift_apply_1 ~f:M.mem

  let add = lift_update_1 ~f:M.add

  let remove = lift_update_1 ~f:M.remove

  let iter = lift_apply_g ~g:M.iter

  let map = lift_update_g ~g:M.map

  let fold x ~init ~f = SM.apply x ~f:(fun x -> M.fold f x init)

  let for_all = lift_apply_g ~g:M.for_all

  let exists = lift_apply_g ~g:M.exists

  let filter = lift_update_g ~g:M.filter

  let cardinal = lift_apply ~f:M.cardinal

  let elements = lift_apply ~f:M.elements

  let min_elt = lift_apply ~f:M.min_elt

  let min_elt_opt = lift_apply ~f:M.min_elt_opt

  let max_elt = lift_apply ~f:M.max_elt

  let max_elt_opt = lift_apply ~f:M.max_elt_opt

  let choose = lift_apply ~f:M.choose

  let choose_opt = lift_apply ~f:M.choose_opt

  let find = lift_apply_1 ~f:M.find

  let find_opt = lift_apply_1 ~f:M.find_opt

  let find_first = lift_apply_g ~g:M.find_first

  let find_first_opt = lift_apply_g ~g:M.find_first_opt

  let find_last = lift_apply_g ~g:M.find_last

  let find_last_opt = lift_apply_g ~g:M.find_last_opt
end
