module IS = InstructionSet
  
module Env = Map.Make(String)

type clo = { code : IS.block; id : string; env : env}
and value =
  | Int of int
  | Clos of clo
  | Unit of unit
and env = value Env.t (* change value here to int in order to define the closures *)

type heap = { mutable memory : (int, value) Hashtbl.t;
              mutable address : int }

let print_reversed_value_list v =
  let value_string value = match value with
    | Int(n) -> (string_of_int n)
    | Clos(clo) -> clo.id
    | Unit(u) -> "()" in
  let rec print_rev_list = function 
  [] -> ()
  | e::l -> print_rev_list l;
            print_string ((value_string e)^" ") in
  print_rev_list v;
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
  mutable env   : env;
  mutable heap  : heap 
}

exception End_of_thread of thread_state
    
let rec step state =
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
      push(v)
      
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
      let thisvalue = pop() in
      state.env <- Env.add id thisvalue state.env;

    | IS.EndLet(id) ->
      state.env <- Env.remove id state.env

    | IS.MkClos(id, e) ->
      push(Clos({code=(Compile.compile_expr e); id=id; env=state.env}))

    | IS.Apply ->
      let v = pop() in
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

    | IS.Alloc ->
      let cur_address = state.heap.address in
      push(Int(cur_address));
      Hashtbl.add state.heap.memory cur_address (Int 0);
      state.heap.address <- cur_address + 1

    | IS.Load ->
      let Int addr = pop() in
      let aux = Hashtbl.find state.heap.memory addr in
      push(aux)

    | IS.Store ->
      let v = pop() in
      let Int addr = pop() in
      Hashtbl.replace state.heap.memory addr v;

    | IS.Unit ->
      push(Unit(()))

    | IS.Dup ->
      let v = pop() in
      push(v);
      push(v)

    | IS.Drop ->
      let Int _ = pop() in
      ()

    | IS.Spawn ->
      let is_empty l = match l with
      | [] -> true
      | _ -> false in
      let v = pop() in
      let Clos(clo) = pop() in
      let new_state = {code=clo.code; stack=[]; env=(Env.add clo.id v Env.empty); heap=state.heap} in
      while not (is_empty new_state.code) do
        step new_state;
        done;
      push(List.hd new_state.stack)

let execute p : unit =
  let is_empty l = match l with
  | [] -> true
  | _ -> false in
  let state = {code=p; stack=[]; env=Env.empty; heap={memory=Hashtbl.create 10; address=0}} in (* should I use 10? *)
  while not (is_empty state.code) do
    step state;
    done;
  print_reversed_value_list state.stack