type +'a t

exception Empty

val empty : 'a t

val add : 'a -> 'a t -> 'a t

val push : 'a -> 'a t -> 'a t

val take : 'a t -> 'a * 'a t

val pop : 'a t -> 'a * 'a t

val peek : 'a t -> 'a

val top : 'a t -> 'a

val is_empty : 'a t -> bool

val length : 'a t -> int

val iter : 'a t -> f:('a -> unit) -> unit

val fold : 'a t -> init:'b -> f:('a -> 'b -> 'b) -> 'b
