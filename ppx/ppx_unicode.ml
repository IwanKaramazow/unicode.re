exception Death


let compile_constant =
  let open Asttypes in
  function
  | Const_int i -> Const_int(i)
  | Const_char c -> Const_int(Char.code c)
  | _ -> raise Death


let compile_cases cases =
  let open Parsetree in
  List.map (function
    | ({pc_lhs} as case) ->
        let ppat_desc = begin match pc_lhs.ppat_desc with
        | Ppat_constant c ->
            Ppat_constant (compile_constant c)
        | Ppat_any -> Ppat_any
        | Ppat_var _ as v -> v
        | _ -> raise Death
        end in
        {case with pc_lhs = {pc_lhs with ppat_desc}}
  ) cases

let lid loc = Asttypes.{txt = Longident.parse "Uchar.toInt"; loc}

let compile_match_expr expr =
  let exprIdent = Parsetree.(
    Ast_helper.Exp.ident ~loc:expr.pexp_loc (lid expr.pexp_loc)
  ) in
  let args = ["", expr] in
  Ast_helper.Exp.apply exprIdent args


let ppx_uchar_mapper _argv =
  { Ast_mapper.default_mapper with
     expr = fun mapper expr ->
       match expr with
       | {pexp_desc = Pexp_extension({txt = "uchar"}, pstr)} ->
         begin match pstr with
         | PStr [{pstr_desc = Pstr_eval(({pexp_desc = Pexp_match(expr, cases)} as e), _)}] ->
             let expr = compile_match_expr expr in
           {e with pexp_desc = Pexp_match(expr, compile_cases cases)}
          | _ -> raise Death
          end
       | e -> Ast_mapper.default_mapper.expr mapper e
  }

let () = Ast_mapper.register "uchar" ppx_uchar_mapper
