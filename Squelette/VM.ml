module IS = InstructionSet
  
module Env = Map.Make(String)
type env = int Env.t (* change value here to int in order to define the closures *)

type clo =
  { code : IS.block;
    id : string;
    env : env}

type value =
  | Int of int
  | Clos of clo

let print_value_list v =
  let value_string value = match value with
    | Int(n) -> (string_of_int n)
    | Clos(clo) -> clo.id in
  let rec print_list = function 
  [] -> ()
  | e::l -> print_string ((value_string e)^" "); print_list l in
  print_list v;
  print_endline " ";

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
      push(Int(v))
      
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

    | IS.Let(id) ->
      let Int n = pop() in
      state.env <- Env.add id n state.env

    | IS.EndLet(id) ->
      state.env <- Env.remove id state.env

    | IS.MkClos(id, e) ->
      push(Clos({code=(Compile.compile_expr e); id=id; env=state.env}))

    | IS.Apply ->
      let Int v = pop() in
      let Clos(clo) = pop() in
      push(Clos({code=state.code; id=""; env=state.env}));
      state.code <- clo.code @ [IS.Return];
      state.env <- Env.add clo.id v clo.env

    | IS.Return ->
      let Int v = pop() in
      let Clos(clo) = pop() in
      push(Int(v));
      state.env <- clo.env;
      state.code <- clo.code

let execute p : unit =
  let is_empty l = match l with
  | [] -> true
  | _ -> false in
  let state = {code=p; stack=[]; env=Env.empty} in
  while not (is_empty state.code) do
    step state;
    done;
  print_value_list state.stack