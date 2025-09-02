type 'a t = { prevs : 'a list; v : 'a; nexts : 'a list; }

let create prevs v nexts = {prevs; v; nexts}

let init v = {prevs= []; v; nexts= []}

let value t = t.v

let prev t n =
  match t.prevs with
  | [] -> create [] n (t.v :: t.nexts)
  | hd :: tl -> create tl hd (t.v :: t.nexts)

let next t n =
  match t.nexts with
  | [] -> create (t.v :: t.prevs) n []
  | hd :: tl -> create (t.v :: t.prevs) hd tl

let set t v = {prevs = t.prevs; v; nexts = t.nexts}

let decr t = {prevs = t.prevs; v = t.v - 1; nexts = t.nexts}

let incr t = {prevs = t.prevs; v = t.v + 1; nexts = t.nexts}
