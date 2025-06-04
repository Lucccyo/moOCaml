type instr =
  | Imoo
  | ImOo
  | ImoO
  | ImOO
  | IMoo
  | IMOo
  | IMoO
  | IMOO
  | IOOO
  | IMMM
  | IOOM
  | Ioom

type state

val pp_state : state -> unit
(** [pp_state s] prints [s]. *)

val init_state : int list -> state
(** [init_state ints] returns an initial state with [ints] as stdin. *)

val exec_prog : instr list -> state -> state
(** [exec_prog instrs s] executes the [instrs] on the state [s] and returns the
    new state. *)
