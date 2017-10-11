type instruction =
  | Int of int
  | Lookup of string
  | Add
  | Sub
  | Mult

and block = instruction list

let instruction_to_string = function
	| Int(n) -> string_of_int n
	| Lookup(id) -> id
	| Add -> "+"
	| Sub -> "-"
	| Mult -> "*"

let print_prog p =
	let print_instruction chan v = output_string chan (instruction_to_string v) in
	let rec print_list = function 
	[] -> ()
	| e::l -> Printf.printf "%a" print_instruction e ; print_string " " ; print_list l in
	print_list p
