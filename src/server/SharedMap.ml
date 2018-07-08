module type KeyS = sig
  include Map.OrderedType

  val pp : Format.formatter -> t -> unit
end

module type ValueS = sig
  type t
end

module type S = sig
  type key

  type value

  type t

  val empty : unit -> t

  val singleton : key -> value -> t

  val is_empty : t -> bool

  val mem : key -> t -> bool

  val add : key -> value -> t -> unit

  val update : key -> f:(value option -> value option) -> t -> unit

  val remove : key -> t -> unit

  val iter : t -> f:(key -> value -> unit) -> unit

  val fold : t -> init:'a -> f:(key -> value -> 'a -> 'a) -> 'a

  val for_all : t -> f:(key -> value -> bool) -> bool

  val exists : t -> f:(key -> value -> bool) -> bool

  val filter : t -> f:(key -> value -> bool) -> unit

  val cardinal : t -> int

  val bindings : t -> (key * value) list

  val min_binding : t -> key * value

  val min_binding_opt : t -> (key * value) option

  val max_binding : t -> key * value

  val max_binding_opt : t -> (key * value) option

  val choose : t -> key * value

  val choose_opt : t -> (key * value) option

  val find : key -> t -> value

  val find_opt : key -> t -> value option

  val find_first : t -> f:(key -> bool) -> key * value

  val find_first_opt : t -> f:(key -> bool) -> (key * value) option

  val find_last : t -> f:(key -> bool) -> key * value

  val find_last_opt : t -> f:(key -> bool) -> (key * value) option

  val pp : Format.formatter -> t -> unit
end

module Make (K : KeyS) (V : ValueS) :
  S with type key = K.t and type value = V.t =
struct
  module M = Map.Make (K)

  module SM = Shared.Make (struct
    type t = V.t M.t
  end)

  type key = K.t

  type value = V.t

  type t = SM.t

  let lift_apply x ~f = SM.apply x ~f

  let lift_apply_1 arg x ~f = SM.apply x ~f:(f arg)

  let lift_update_1 arg x ~f = SM.update x ~f:(f arg)

  let lift_update_2 arg1 arg2 x ~f = SM.update x ~f:(f arg1 arg2)

  let lift_apply_g ~g x ~f = SM.apply x ~f:(g f)

  let lift_update_g ~g x ~f = SM.update x ~f:(g f)

  let empty () = SM.init M.empty

  let singleton k v = SM.init (M.singleton k v)

  let is_empty = lift_apply ~f:M.is_empty

  let mem = lift_apply_1 ~f:M.mem

  let add = lift_update_2 ~f:M.add

  let update k ~f x = SM.update x ~f:(M.update k f)

  let remove = lift_update_1 ~f:M.remove

  let iter = lift_apply_g ~g:M.iter

  let fold x ~init ~f = SM.apply x ~f:(fun x -> M.fold f x init)

  let for_all = lift_apply_g ~g:M.for_all

  let exists = lift_apply_g ~g:M.exists

  let filter = lift_update_g ~g:M.filter

  let cardinal = lift_apply ~f:M.cardinal

  let bindings = lift_apply ~f:M.bindings

  let min_binding = lift_apply ~f:M.min_binding

  let min_binding_opt = lift_apply ~f:M.min_binding_opt

  let max_binding = lift_apply ~f:M.max_binding

  let max_binding_opt = lift_apply ~f:M.max_binding_opt

  let choose = lift_apply ~f:M.choose

  let choose_opt = lift_apply ~f:M.choose_opt

  let find = lift_apply_1 ~f:M.find

  let find_opt = lift_apply_1 ~f:M.find_opt

  let find_first = lift_apply_g ~g:M.find_first

  let find_first_opt = lift_apply_g ~g:M.find_first_opt

  let find_last = lift_apply_g ~g:M.find_last

  let find_last_opt = lift_apply_g ~g:M.find_last_opt

  let pp fmt x =
    let pp' fmt x =
      Format.fprintf fmt "[@[" ;
      if not (M.is_empty x) then (
        let k, _v = M.min_binding x in
        Format.fprintf fmt "%a" K.pp k ;
        let x' = M.remove k x in
        M.iter (fun k _v -> Format.fprintf fmt ",@;%a" K.pp k) x' ) ;
      Format.fprintf fmt "@]]"
    in
    SM.apply x ~f:(fun x -> pp' fmt x)
end
