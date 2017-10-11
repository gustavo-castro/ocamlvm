let usage = "usage: ./VM file.cml"
let spec  = []
  
let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".cml") then
      raise (Arg.Bad "no .cml extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> exit 1

let () =
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  let e  = Parser.main Lexer.token lb in
  close_in c;
  let p  = Compile.compile_expr e in
  InstructionSet.print_prog p;
  VM.execute p; 
  exit 0
