open InstructionSet

let rec compile_expr = function
  | Ast.Int(n) ->
    [Int(n)]

  | Ast.Ident(id) ->
    [Lookup(id)]

  | Ast.Binop(Ast.Add, e1, e2) ->
    (* D'abord un opérande, puis
       l'autre, puis l'opérateur,
       comme en notation
       polonaise inversée. *)
    (compile_expr e2)
    @ (compile_expr e1)
    @ [Add]

  | Ast.Binop(Ast.Sub, e1, e2) ->
    (compile_expr e2)
    @ (compile_expr e1)
    @ [Sub]

  | Ast.Binop(Ast.Mult, e1, e2) ->
    (compile_expr e2)
    @ (compile_expr e1)
    @ [Mult]

  | Ast.Letin(id, e1, e2) ->
    (compile_expr e1)
    @ [Let(id)]
    @ (compile_expr e2)
    @ [EndLet(id)]