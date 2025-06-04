type instr =
  | Imoo
      (** This command is connected to the MOO command. When encountered during
          normal execution, it searches the program code in reverse looking for
          a matching MOO command and begins executing again starting from the
          found MOO command. When searching, it skips the instruction that is
          immediately before it (see MOO). *)
  | ImOo  (** Moves current memory position back one block. *)
  | ImoO  (** Moves current memory position forward one block. *)
  | ImOO
      (** Execute value in current memory block as if it were an instruction.
          The command executed is based on the instruction code value (for
          example, if the current memory block contains a 2, then the moO
          command is executed). An invalid command exits the running program.
          Value 3 is invalid as it would cause an infinite loop. *)
  | IMoo
      (** If current memory block has a 0 in it, read a single ASCII character
          from STDIN and store it in the current memory block. If the current
          memory block is not 0, then print the ASCII character that corresponds
          to the value in the current memory block to STDOUT.*)
  | IMOo  (** Decrement current memory block value by 1. *)
  | IMoO  (** Increment current memory block value by 1. *)
  | IMOO
      (** If current memory block value is 0, skip next command and resume
          execution after the next matching moo command. If current memory block
          value is not 0, then continue with next command. Note that the fact
          that it skips the command immediately following it has interesting
          ramifications for where the matching moo command really is. For
          example, the following will match the second and not the first moo:
          OOO MOO moo moo *)
  | IOOO  (** Set current memory block value to 0. *)
  | IMMM
      (** If no current value in register, copy current memory block value. If
          there is a value in the register, then paste that value into the
          current memory block and clear the register. *)
  | IOOM  (** Print value of current memory block to STDOUT as an integer. *)
  | Ioom
      (** Read an integer from STDIN and put it into the current memory block.
      *)

type state = {
  memory : Dllist.t;
  stdin : int list;
  stdout : char list;
  buffer : int option;
}

let pp_state (s : state) : unit =
  let show_int_list label lst =
    Format.printf "%s: [ " label;
    List.iter (Format.printf "%d ") lst;
    Format.printf "]@."
  in
  let show_char_list label lst =
    Format.printf "%s :'" label;
    List.iter (Format.printf "%c") lst;
    Format.printf "'@."
  in
  let show_option label = function
    | None -> Format.printf "%s: None@." label
    | Some x -> Format.printf "%s: Some %d@." label x
  in
  Dllist.pp s.memory;
  show_int_list "stdin" s.stdin;
  show_char_list "stdout" s.stdout;
  show_option "buffer" s.buffer;
  Format.printf "@."

let int_to_instr n =
  match n with
  | 0 -> None (* Imoo -> infinite loop *)
  | 1 -> Some ImOo
  | 2 -> Some ImoO
  | 3 -> None (* ImOO -> infinite loop *)
  | 4 -> Some IMoo
  | 5 -> Some IMOo
  | 6 -> Some IMoO
  | 7 -> None (* IMOO -> infinite loop *)
  | 8 -> Some IOOO
  | 9 -> Some IMMM
  | 10 -> Some IOOM
  | 11 -> Some Ioom
  | _ -> None

let int_to_ascii = Char.chr
let int_to_intchar_list n = String.to_seq (string_of_int n) |> List.of_seq

let collect_loops p =
  let rec aux p i stack acc =
    match p with
    | [] -> acc
    | instr :: rest -> (
        match instr with
        | IMOO -> aux rest (i + 1) (i :: stack) acc
        | Imoo -> (
            match stack with
            | start :: tail -> aux rest (i + 1) tail ((start, i) :: acc)
            | [] -> failwith "Unmatched Imoo")
        | _ -> aux rest (i + 1) stack acc)
  in
  aux p 0 [] []

let make_loop_jumps p =
  let pairs = collect_loops p in
  let t = Hashtbl.create 10 in
  List.iter
    (fun (start, stop) ->
      Hashtbl.add t start (stop + 1);
      Hashtbl.add t stop start)
    pairs;
  t

let rec exec_prog p pc s jumps =
  if pc >= List.length p then s
  else
    let i = List.nth p pc in
    let next_pc, new_state =
      match i with
      | IMOO ->
          if Dllist.value s.memory = 0 then (Hashtbl.find jumps pc, s)
          else (pc + 1, s)
      | Imoo ->
          if Dllist.value s.memory <> 0 then (Hashtbl.find jumps pc, s)
          else (pc + 1, s)
      | ImOo -> (pc + 1, { s with memory = Dllist.prev s.memory 0 })
      | ImoO -> (pc + 1, { s with memory = Dllist.next s.memory 0 })
      | IMoo ->
          let memory_v = Dllist.value s.memory in
          if memory_v = 0 then
            ( pc + 1,
              {
                memory = Dllist.set s.memory (List.hd s.stdin);
                stdin = List.tl s.stdin;
                stdout = s.stdout;
                buffer = s.buffer;
              } )
          else
            ( pc + 1,
              {
                memory = s.memory;
                stdin = s.stdin;
                stdout = int_to_ascii memory_v :: s.stdout;
                buffer = s.buffer;
              } )
      | IMOo -> (pc + 1, { s with memory = Dllist.decr s.memory })
      | IMoO -> (pc + 1, { s with memory = Dllist.incr s.memory })
      | ImOO -> (
          match int_to_instr (Dllist.value s.memory) with
          | Some inst ->
              let temp_state = exec_prog [ inst ] 0 s jumps in
              (pc + 1, temp_state)
          | None -> (pc + 1, s))
      | IOOO -> (pc + 1, { s with memory = Dllist.set s.memory 0 })
      | IMMM -> (
          match s.buffer with
          | Some e ->
              ( pc + 1,
                {
                  memory = Dllist.set s.memory e;
                  stdin = s.stdin;
                  stdout = s.stdout;
                  buffer = None;
                } )
          | None -> (pc + 1, { s with buffer = Some (Dllist.value s.memory) }))
      | IOOM ->
          ( pc + 1,
            {
              s with
              stdout = int_to_intchar_list (Dllist.value s.memory) @ s.stdout;
            } )
      | Ioom ->
          if s.stdin = [] then failwith "Stdin is empty, unable to read";
          let e = List.hd s.stdin in
          ( pc + 1,
            {
              memory = Dllist.set s.memory e;
              stdin = List.tl s.stdin;
              stdout = s.stdout;
              buffer = None;
            } )
    in
    exec_prog p next_pc new_state jumps

let exec_prog p s =
  let jumps = make_loop_jumps p in
  let s = exec_prog p 0 s jumps in
  { s with stdout = List.rev s.stdout }

let init_state l =
  { memory = Dllist.init 0; stdin = l; stdout = []; buffer = None }
