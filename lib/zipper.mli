type 'a t

val create : 'a list -> 'a -> 'a list -> 'a t
(** [create prev v next] creates a double linked list with [v] as value and [prev] and [next] as neighbours. *)

val init : 'a -> 'a t
(** [init v] initialise a double linked list with [v] as value. *)

val value : 'a t -> 'a
(** Returns the current cell's value. *)

val prev : 'a t -> 'a -> 'a t
(** [prev t n] returns the previous cell of [t]. If this cell is empty, it creates a new cell with [n] as value. *)

val next : 'a t -> 'a -> 'a t
(** [next t n] returns the next cell of [t]. If this cell is empty, it creates a new cell with [n] as value. *)

val set : 'a t -> 'a -> 'a t
(** [set t n] returns [t] with [n] as current value. *)

val decr : int t -> int t
(** Returns a new [t] where the value is decremented by 1. *)

val incr : int t -> int t
(** Returns a new [t] where the value is incremented by 1. *)