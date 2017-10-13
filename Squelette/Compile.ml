open InstructionSet

let rec compile_expr = function
  | Ast.Int(n) ->
    [Int(n)]

  | Ast.Ident(id) ->
    [Lookup(id)]

  | Ast.GetR(e) ->
    (compile_expr e)
    @ [Load]

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

  | Ast.Fun(id, e) ->
    [MkClos(id, e)]

  | Ast.Apply(e1, e2) ->
    (compile_expr e1)
    @ (compile_expr e2)
    @ [Apply]

  | Ast.Seq(e1, e2) ->
    (compile_expr e1)
    @ (compile_expr e2)

  | Ast.Ref(e) -> 
    [Alloc]
    @ [Dup]
    @ (compile_expr e)
    @ [Store]

  | Ast.SetR(d, e) -> (* fazer ainda *)
    (compile_expr d)
    @ (compile_expr e)
    @ [Store]
    @ [Unit]