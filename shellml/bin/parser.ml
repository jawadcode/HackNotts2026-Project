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
      "Bool";
      "Int";
      "String";
      "List";
      "of";
      "Fun";
      "And";
      "Or";
      "unit";
      "true";
      "false";
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
  let* i = take_while Char.is_digit |> lexeme in
  let+ i = int_of_string i in
  IntLit i

let raw_ident =
  let* i = name in
  if Set.mem i keywords then fail ("expected ident, got keyword " ^ i)
  else return i

(* N.B. let* is just do-notation *)

let ident =
  let* i = name in
  if Set.mem i keywords then fail ("expected ident, got keyword " ^ i)
  else return @@ Ident i

let keyword kw =
  let* i = lexeme name in
  if i = kw then return () else fail ("expected keyword " ^ kw)

(* Ignore parens (operators point away from them) *)
let parenthesise p = string "(" *> p <* string ")"

let list_literal elem =
  let+ l = string "[" *> many elem <* string "]" in
  List l

let ty =
  let simple =
    choice
      [
        keyword "Unit" *> return UnitTy;
        keyword "Bool" *> return BoolTy;
        keyword "Int" *> return IntTy;
        keyword "String" *> return StringTy;
      ]
  in
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
      simple <|> list_ty <|> fun_ty)

let lam expr =
  keyword "fun" *> string "("
  *> lift4
       (fun param param_ty ret_ty body -> Lam { param; param_ty; ret_ty; body })
       raw_ident
       (string ":" *> ty <* string ")")
       (string ":" *> ty)
       (string "->" *> expr)

let let_bind expr =
  let binding =
    lift3
      (fun name ty value -> (name, ty, value))
      raw_ident
      (string ":" *> ty)
      (string "=" *> expr)
  in
  keyword "let"
  *> lift2
       (fun bindings body -> Let (bindings, body))
       ((* sep_by1 (string ";") *) List.singleton <$> binding)
       expr

let if_else expr =
  keyword "if"
  *> lift3
       (fun cond then_ else_ -> IfElse (cond, then_, else_))
       expr
       (keyword "then" *> expr)
       (keyword "else" *> expr)

let ( <?> ) p l =
  let* remaining = available in
  let remaining = min remaining 20 in
  let* s = peek_string remaining in
  p <?> Printf.sprintf "%s, got: [%s]" l s

let expr_parser =
  fix (fun expr_parser ->
      let simple =
        choice
          [
            parenthesise expr_parser;
            list_literal expr_parser;
            keyword "unit" *> return Unit;
            keyword "true" *> return True;
            keyword "false" *> return False;
            integer;
            ident;
          ]
        <?> "simple expression"
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
        (fun left (* args *) ops ->
          (* let left = *)
          (*   List.fold_left (fun acc arg -> Call (acc, arg)) left args *)
          (* in *)
          List.fold_right
            (fun (op, right) acc -> BinOp (acc, op, right))
            ops left)
        simple
        (* (simple |> many) *) (many op_ext)
      <|> let_bind expr_parser <|> lam expr_parser <|> if_else expr_parser
      (* <?> associates name with parser *)
      <?> "complex expression")

let item_parser =
  let autocurry name params ret_ty body =
    (* It was pain and suffering to compute these return types *)
    let fold_ret_ty tys =
      List.fold_right (fun ty acc -> FunTy (ty, acc)) tys ret_ty
    in
    let tys = params |> List.tl |> List.map snd in
    let ret_tys = tys |> unconsing |> List.map fold_ret_ty in
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
  let param = both raw_ident (string ":" *> ty) in
  whitespace
  *> lift4
       (fun name params ret_ty body -> autocurry name params ret_ty body)
       (keyword "fun" *> raw_ident)
       (sep_by1 (string ",") param |> parenthesise)
       (string ":" *> ty)
       (string "=" *> expr_parser)

(* val add3 : int -> string -> float -> int *)

(* let add3 (x : int) : string -> float -> int = *)
(*  fun (y : string) : (float -> int) -> fun (z : float) : int -> 123 *)

let run_parser p = parse_string ~consume:All p
