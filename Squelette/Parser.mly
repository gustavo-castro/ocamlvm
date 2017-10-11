%{
  open Ast
%}

%token <int> INT
%token <string> IDENT
%token PLUS MINUS STAR
%token LP RP
%token LET EQ IN
%token FUN ARROW
%token IF THEN ELSE
%token WHILE DO DONE
%token SEMI
%token REF BANG SET
%token SPAWN
%token EOF

%nonassoc IN
%nonassoc ARROW
%left SEMI
%nonassoc ELSE
%nonassoc SET
%left PLUS MINUS
%left STAR
%nonassoc LP INT IDENT
%nonassoc BANG

%start main
%type <Ast.expr> main

%%

main:
| e=expr EOF { e }
;

expr:
| e=simple_expr                        { e                 }    
| e1=expr op=binop e2=expr             { Binop(op, e1, e2) }
| LET id=IDENT EQ e1=expr IN e2=expr   { Letin(id, e1, e2) }
| FUN id=IDENT ARROW e=expr            { Fun(id, e)        }
| e1=expr e2=simple_expr               { Apply(e1, e2)     }
| IF c=expr THEN e1=expr ELSE e2=expr  { Cond(c, e1, e2)   }
| WHILE c=expr DO e=expr DONE          { Loop(c, e)        }
| e1=expr SEMI e2=expr                 { Seq(e1, e2)       }
| REF e=simple_expr                    { Ref(e)            }
| d=expr SET e=expr                    { SetR(d, e)        }
| SPAWN e1=simple_expr e2=simple_expr  { Spawn(e1, e2)     }
;

simple_expr:
| n=INT              { Int(n)   }
| id=IDENT           { Ident(id)}
| LP e=expr RP       { e        }
| BANG e=simple_expr { GetR(e)  }
;

%inline binop:
| PLUS  { Add  }
| MINUS { Sub  }
| STAR  { Mult }
;
