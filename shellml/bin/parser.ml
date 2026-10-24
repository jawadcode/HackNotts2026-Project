open Angstrom
open Batteries
open Ast

let keywords =
  Set.of_list
    [
      "fun";
      "let";
      "in";
      "if";
      "then";
      "else";
      "Unit";
      "Int";
      "String";
      "List";
      "of";
      "Fun";
    ]

let whitespace = take_while Char.is_whitespace
let lexeme parser = parser <* whitespace

let name =
  let is_ident_char c = Char.is_letter c || c == '_' in
  take_while is_ident_char

let string s = lexeme (string s) *> return ()

let integer =
  let+ i = take_while Char.is_digit in
  IntLit (Int.of_string i)

(* N.B. let* is just do-notation *)

let ident =
  let* i = name in
  if Set.mem i keywords then fail ("expected ident, got keyword " ^ i)
  else return @@ Ident i

let keyword kw =
  let* i = name in
  if i == kw then return () else fail ("expected keyword " ^ kw)

(* Ignore parens (operators point away from them) *)
let parens p = string "(" *> p <* string ")"

let expr_parser =
  fix (fun expr_parser ->
      let simple = choice [ parens expr_parser; integer; ident ] in
      (* <?> associates name with parser *)
      simple <?> "expression")

let item_parser =
  let ty =
    fix (fun ty_parser ->
        let list_ty =
          let+ list_type = keyword "List" *> keyword "of" *> ty_parser in
          ListTy list_type
        in
        let fun_ty =
          let+ in_ty, out_ty =
            keyword "Fun" *> keyword "of"
            *> both ty_parser (string "->" *> ty_parser)
          in
          FunTy (in_ty, out_ty)
        in
        choice
          [
            keyword "Unit" *> return UnitTy;
            keyword "Int" *> return IntTy;
            keyword "String" *> return StringTy;
            list_ty;
            fun_ty;
          ])
  in
  let param = both name (string ":" *> ty) in
  lift4
    (fun name params ret_ty body -> ItemFun { name; params; ret_ty; body })
    (keyword "fun" *> name)
    (many1 param |> parens)
    (string ":" *> ty)
    expr_parser

let run_parser p = parse_string ~consume:All p
