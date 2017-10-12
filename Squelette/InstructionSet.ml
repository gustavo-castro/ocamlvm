type instruction =
  | Int of int
  | Lookup of string
  | Add
  | Sub
  | Mult
  | Let of string
  | EndLet of string

and block = instruction list

let instruction_to_string = function
	| Int(n) -> string_of_int n
	| Lookup(id) -> id
	| Add -> "+"
	| Sub -> "-"
	| Mult -> "*"
	| Let(id) -> "Let"
	| EndLet(id) -> "EndLet"

let print_prog p =
	let rec print_list = function 
	[] -> ()
	| e::l -> print_string ((instruction_to_string e)^" "); print_list l in
	print_list p;
	print_endline " ";