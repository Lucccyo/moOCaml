type t =
  | Empty
  | Cell of { mutable prev : t; v : int; mutable next : t }

let rec pp_prevs t =
  match t with
  | Empty -> Printf.printf "[]%!"
  | Cell c ->
      pp_prevs c.prev;
      Printf.printf "[%d]%!" c.v

let rec pp_nexts t =
  match t with
  | Empty -> Printf.printf "[]%!"
  | Cell c ->
      Printf.printf "[%d]%!" c.v;
      pp_nexts c.next

let pp t =
  match t with
  | Empty -> Format.printf "[]@."
  | Cell c ->
      pp_prevs c.prev;
      Printf.printf "[<%d>]%!" c.v;
      pp_nexts c.next;
      Format.printf "@."

let create prev v next = Cell { prev; v; next }

let init v = create Empty v Empty

let value t = match t with Empty -> failwith "Dllist is Empty" | Cell c -> c.v

let set t x =
  match t with
  | Empty -> failwith "Dllist is Empty"
  | Cell c ->
      create c.prev x c.next

let prev t x =
  match t with
  | Empty -> init x
  | Cell { prev = Empty; _ } -> (
      let new_cell = init x in
      match new_cell with
      | Empty -> assert false
      | Cell c ->
          c.next <- t;
          (match t with
          | Empty -> assert false
          | Cell ct -> ct.prev <- new_cell);
          new_cell)
  | Cell c -> c.prev

let next t x =
  match t with
  | Empty -> init x
  | Cell { next = Empty; _ } -> (
      let new_cell = init x in
      match new_cell with
      | Empty -> assert false
      | Cell nc ->
          nc.prev <- t;
          (match t with
          | Empty -> assert false
          | Cell ct -> ct.next <- new_cell);
          new_cell)
  | Cell c -> c.next

let incr t =
  match t with
  | Empty -> failwith "Dllist is Empty"
  | Cell c ->
      create c.prev (c.v + 1) c.next

let decr t =
  match t with
  | Empty -> failwith "Dllist is Empty"
  | Cell c ->
      create c.prev (c.v - 1) c.next
