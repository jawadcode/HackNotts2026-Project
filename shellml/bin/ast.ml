type item =
  | ItemFun of {
      name : string;
      params : (string * ty) list;
      ret_ty : ty;
      body : expr;
    }

and expr =
  | Unit
  | IntLit of int
  | StringLit of string
  | Ident of string
  | List of expr list
  | BinOp of expr * bop * expr
  | Call of expr * expr
  | Let of (string * ty * expr) list * expr
  | IfElse of expr * expr * expr

and ty = UnitTy | BoolTy | IntTy | StringTy | ListTy of ty | FunTy of ty * ty

(* TODO: Add int comparison ops *)
and bop =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Less
  | Greater
  | And
  | Or
  | Cons
  | Concat

(* The things we do for pretty output *)
let join sep strings =
  match strings with
  | [] -> failwith "nuh uh"
  | [ sole ] -> sole
  | head :: tail ->
      head ^ List.fold_left (fun acc param -> acc ^ sep ^ param) "" tail

(* ppx_show requires a pre-historic OCaml version 😠 *)
let rec string_of_item (ItemFun { name; params; ret_ty; body }) =
  let string_of_params params =
    let string_of_param (name, ty) =
      Printf.sprintf "%s: %s" name (string_of_type ty)
    in
    List.map string_of_param params |> join ", "
  in
  Printf.sprintf "fun %s(%s) : %s = %s" name (string_of_params params)
    (string_of_type ret_ty) (string_of_expr body)

and string_of_type = function
  | UnitTy -> "Unit"
  | BoolTy -> "Bool"
  | IntTy -> "Int"
  | StringTy -> "String"
  | ListTy inner -> "List of " ^ string_of_type inner
  | FunTy (in_ty, out_ty) ->
      Printf.sprintf "Fun of %s -> %s" (string_of_type in_ty)
        (string_of_type out_ty)

and string_of_expr = function
  | Unit -> "Unit"
  | IntLit n -> Int.to_string n
  | StringLit s -> "\"" ^ s ^ "\""
  | Ident name -> name
  | List exprs -> List.map string_of_expr exprs |> join "; "
  | BinOp (lhs, op, rhs) ->
      Printf.sprintf "(%s %s %s)" (string_of_expr lhs) (string_of_bop op)
        (string_of_expr rhs)
  | Call (f, arg) ->
      Printf.sprintf "(%s %s)" (string_of_expr f) (string_of_expr arg)
  | Let (bindings, body) ->
      let string_of_bindings bindings =
        let string_of_binding (name, ty, value) =
          Printf.sprintf "%s: %s = %s" name (string_of_type ty)
            (string_of_expr value)
        in
        List.map string_of_binding bindings |> join "; "
      in
      Printf.sprintf "let %s in %s"
        (string_of_bindings bindings)
        (string_of_expr body)
  | IfElse (cond, then_, else_) ->
      Printf.sprintf "if %s then %s else %s" (string_of_expr cond)
        (string_of_expr then_) (string_of_expr else_)

and string_of_bop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | And -> "and"
  | Or -> "or"
  | Eq -> "=="
  | Less -> "<"
  | Greater -> ">"
  | Cons -> "::"
  | Concat -> "++"
