type t

val pp : t -> unit

val create : t -> int -> t -> t
(** [create prev v next] creates a double linked list with [v] as value and [prev] and [next] as neighbours. *)

val init : int -> t
(** [init v] initialise a double linked list with [v] as value. *)

val value : t -> int
(** Returns the current cell's value. *)

val prev : t -> int -> t
(** [prev t n] returns the previous cell of [t]. If this cell is empty, it
    creates a new cell with [n] as value. *)

val next : t -> int -> t
(** [next t n] returns the next cell of [t]. If this cell is empty, it creates a
    new cell with [n] as value. *)

val set : t -> int -> t
(** [set t n] returns [t] with [n] as current value. *)

val decr : t -> t
(** Returns the next cell of [t]. *)

val incr : t -> t
(** Returns the next cell of [t]. *)
