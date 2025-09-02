type 'a t = {
  prevs : 'a list;
  v : 'a;
  nexts : 'a list;
}


(* val create : 'a t -> int -> 'a t -> 'a t *)
(** [create prev v next] creates a double linked list with [v] as value and [prev] and [next] as neighbours. *)
(* assuming that prevs is already reversed *)
let create prevs v nexts = {prevs; v; nexts}

(* val init : int -> t *)
(** [init v] initialise a double linked list with [v] as value. *)
let init v = {prevs= []; v; nexts= []}

(* val value : t -> int *)
(** Returns the current cell's value. *)
let value t = t.v

(* val prev : t -> int -> t *)
(** [prev t n] returns the previous cell of [t]. If this cell is empty, it
    creates a new cell with [n] as value. *)
let prev t n =
  match t.prevs with
  | [] -> create [] n (t.v :: t.nexts)
  | hd :: tl -> create tl hd (t.v :: t.nexts)

(* val next : t -> int -> t *)
(** [next t n] returns the next cell of [t]. If this cell is empty, it creates a
    new cell with [n] as value. *)
let next t n =
  match t.nexts with
  | [] -> create (t.v :: t.prevs) n []
  | hd :: tl -> create (t.v :: t.prevs) hd tl


(* val set : t -> int -> t *)
(** [set t n] returns [t] with [n] as current value. *)
let set t v =
  {prevs = t.prevs; v; nexts = t.nexts}

(* val decr : t -> t *)
let decr t = {prevs = t.prevs; v = t.v - 1; nexts = t.nexts}
let incr t = {prevs = t.prevs; v = t.v + 1; nexts = t.nexts}


(* val incr : t -> t *)
(** Returns the next cell of [t]. *)

