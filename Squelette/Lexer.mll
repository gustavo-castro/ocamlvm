{
  open Lexing
  open Parser

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (k, t) -> Hashtbl.add h k t)
      [ "let",  LET;
	"in",   IN;
	"fun",  FUN;
	"if",   IF;
	"then", THEN;
	"else", ELSE;
	"while", WHILE;
	"do",   DO;
	"done", DONE;
	"ref",  REF;
	"spawn", SPAWN;
      ] ;
    fun s ->
      try Hashtbl.find h s
      with Not_found -> IDENT(s)
}

let digit = ['0'-'9']
let alpha = ['a'-'z']
  
rule token = parse
  | ['\n' ' ' '\t' '\r']+  { token lexbuf      }
  | digit+ { INT(int_of_string(lexeme lexbuf)) }
  | alpha+ { id_or_keyword(lexeme lexbuf)      }
  | "+"  { PLUS  }
  | "-"  { MINUS }
  | "*"  { STAR  }
  | "("  { LP    }
  | ")"  { RP    }
  | "="  { EQ    }
  | "->" { ARROW }
  | ";"  { SEMI  }
  | "!"  { BANG  }
  | ":=" { SET   } (* changed set to := (from <-) for it to be equivalent to ocaml*)
  | "(*" { comment lexbuf; token lexbuf }
  | _    { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
  | eof  { EOF   }

and comment = parse
  | "*)" { () }
  | eof  { failwith "Open comment" }
  | _    { comment lexbuf }
