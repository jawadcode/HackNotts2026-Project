open Angstrom
open Batteries
open Ast
open Utils

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
      "And";
      "Or";
    ]

let whitespace = take_while Char.is_whitespace
let lexeme parser = parser <* whitespace

let name =
  let is_ident_char c = Char.is_letter c || c = '_' in
  take_while is_ident_char |> lexeme

let string s = lexeme (string s) *> return ()

let int_of_string i =
  try Int.of_string i |> return
  with _ -> fail "int of string conversion failed"

let integer =
  let* i = take_while Char.is_digit in
  let+ i = int_of_string i in
  IntLit i

(* N.B. let* is just do-notation *)

let ident =
  let* i = name in
  if Set.mem i keywords then fail ("expected ident, got keyword " ^ i)
  else return @@ Ident i

let keyword kw =
  print_endline (dump kw);
  let* i = lexeme name in
  if i = kw then return () else fail ("expected keyword " ^ kw)

(* Ignore parens (operators point away from them) *)
let parenthesise p = string "(" *> p <* string ")"

let list_literal elem =
  let+ l = string "[" *> many elem <* string "]" in
  List l

let expr_parser =
  fix (fun expr_parser ->
      let simple =
        choice
          [ parenthesise expr_parser; list_literal expr_parser; integer; ident ]
      in
      (* Who even needs parser precedence anyways 😎 *)
      let op_ext =
        both
          (choice
             [
               string "+" *> return Add;
               string "-" *> return Sub;
               string "*" *> return Mul;
               string "/" *> return Div;
               string "==" *> return Eq;
               string "<" *> return Less;
               string ">" *> return Greater;
               keyword "and" *> return And;
               keyword "or" *> return Or;
               string "::" *> return Cons;
               string "++" *> return Concat;
             ])
          simple
      in
      lift2
        (fun left ops ->
          if List.is_empty ops then left
          else
            List.fold_left
              (fun acc (op, right) -> BinOp (acc, op, right))
              left ops)
        simple (op_ext |> many)
      (* <?> associates name with parser *)
      <?> "expression")

let item_parser =
  let ty =
    fix (fun ty_parser ->
        let list_ty =
          let+ list_type = keyword "List" *> keyword "of" *> ty_parser in
          ListTy list_type
        and fun_ty =
          let+ in_ty, out_ty =
            keyword "Fun" *> keyword "of"
            *> both ty_parser (string "->" *> ty_parser)
          in
          FunTy (in_ty, out_ty)
        in
        choice
          [
            keyword "Unit" *> return UnitTy;
            keyword "Bool" *> return BoolTy;
            keyword "Int" *> return IntTy;
            keyword "String" *> return StringTy;
            list_ty;
            fun_ty;
          ])
  and autocurry name params ret_ty (body : expr) =
    (* It was pain and suffering to compute these return types *)
    let fold_ret_ty tys =
      List.fold_right (fun ty acc -> FunTy (ty, acc)) tys ret_ty
    in
    let tys = params |> List.tl |> List.map snd in
    let ret_tys = tys |> unconsing |> List.map fold_ret_ty in
    List.iter (fun ty -> print_endline (string_of_type ty)) ret_tys;
    match (params, ret_tys) with
    | [], [] -> failwith "impossibru"
    | head :: tail, ret_tys ->
        ItemFun
          {
            name;
            param = head;
            ret_ty = fold_ret_ty tys;
            body =
              (let ts = List.combine tail ret_tys in
               List.fold_right
                 (fun ((param, param_ty), ret_ty) acc ->
                   Lam { param; param_ty; ret_ty; body = acc })
                 ts body);
          }
    | _ -> failwith "zoinks"
  in
  let param = both name (string ":" *> ty) in
  whitespace
  *> lift4
       (fun name params ret_ty body -> autocurry name params ret_ty body)
       (keyword "fun" *> name)
       (sep_by1 (string ",") param |> parenthesise)
       (string ":" *> ty)
       (string "=" *> expr_parser)

(* val add3 : int -> string -> float -> int *)

(* let add3 (x : int) : string -> float -> int = *)
(*  fun (y : string) : (float -> int) -> fun (z : float) : int -> 123 *)

let run_parser p = parse_string ~consume:All p
