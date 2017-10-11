module IS = InstructionSet

type value =
  | Int of int
  
module Env = Map.Make(String)
type env = value Env.t

(* Ici, version immuable *)
(*
type thread_state = {
  code  : block;
  stack : value list;
  env   : env
}
*)

type thread_state = {
  mutable code  : IS.block;
  mutable stack : value list;
  mutable env   : env
}

exception End_of_thread of thread_state
    
let step state =
  let fetch() =
    match state.code with
      | []   ->
	raise (End_of_thread state)
      | i::c ->
	state.code <- c;
	i
  in
  let push v =
    state.stack <- v::state.stack
  in
  let pop() =
    match state.stack with
      | [] -> assert false
      | v::s ->
	state.stack <- s;
	v
  in
  match fetch() with
    | IS.Int(n) ->
      push (Int n)
	
    | IS.Lookup(id) ->
      let v =
	Env.find id state.env
      in
      push v
      
    | IS.Add ->
      let Int n1 = pop() in
      let Int n2 = pop() in
      push(Int(n1+n2))

    | IS.Sub ->
      let Int n1 = pop() in
      let Int n2 = pop() in
      push(Int(n1-n2))

    | IS.Mult ->
      let Int n1 = pop() in
      let Int n2 = pop() in
      push(Int(n1*n2))

let execute p : unit =
  let is_empty l = match l with
  | [] -> true
  | _ -> false in
  let state = {code=p; stack=[]; env=Env.empty} in
  while not (is_empty state.code) do
    step state;
    done;
  print_int (IS.inst_to_int (List.hd state.code))