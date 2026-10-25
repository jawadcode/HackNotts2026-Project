open Batteries
open Ast
open Result.Syntax

type typ =
  | TUnit
  | TBool
  | TInt
  | TString
  | TList of typ
  | TListOfT
  | TFun of typ * typ

let rec typ_of_ty = function
  | UnitTy -> TUnit
  | BoolTy -> TBool
  | IntTy -> TInt
  | StringTy -> TString
  | ListTy elem -> TList (typ_of_ty elem)
  | FunTy (in_ty, out_ty) -> TFun (typ_of_ty in_ty, typ_of_ty out_ty)

(* Utterly useless because angstrom does not produce bytespans or any form of location/position information 😫 *)
type type_err = type_err' * (* Context: *) string

and type_err' =
  | TEMismatch of { expected : typ; inferred : typ }
  | TEUnbound of string

let lookup var = List.find_opt (fun (name, ty) -> var = name) %> Option.map snd
let insert var ty = ( :: ) (var, ty)

let rec check_item env (ItemFun { name; params; ret_ty; body }) =
  let env = List.map (fun (name, ty) -> (name, typ_of_ty ty)) params @ env in
  check_expr name env (typ_of_ty ret_ty) body

(** `ctx` is an attempt to salvage some meaningful location info *)
and check_expr ctx env expected expr =
  let* inferred = infer_expr ctx env expected expr in
  if inferred = expected then Ok ()
  else Error (ctx, TEMismatch { expected; inferred })

and infer_expr ctx env expected expr =
  let expect_list () =
    match expected with
    | TList _ | TListOfT -> Ok expected
    | _ -> Error (ctx, TEMismatch { expected; inferred = TListOfT })
  in
  let infer_list = function
    | [] -> expect_list ()
    | head :: tail -> (
        let* head_ty = infer_expr ctx env expected head in
        List.find_map_opt
          (fun elem ->
            match check_expr ctx env head_ty elem with
            | Ok () -> None
            | Error error -> Some error)
          tail
        |> function
        | Some (ctx, err) -> Error (ctx, err)
        | None -> Ok (TList head_ty))
  in

  match expr with
  | Unit -> Ok TUnit
  | IntLit _ -> Ok TInt
  | StringLit s -> Ok TString
  | Ident name ->
      lookup name env |> Stdlib.Option.to_result ~none:(ctx, TEUnbound name)
  | List elems -> infer_list elems
  | BinOp (lhs, op, rhs) -> (
      match op with
      | Add | Sub | Mul | Div ->
          let* _ = check_expr ctx env TInt lhs in
          let+ _ = check_expr ctx env TInt rhs in
          TInt
      | And | Or ->
          let* _ = check_expr ctx env TBool lhs in
          let+ _ = check_expr ctx env TBool rhs in
          TBool
      | Cons ->
          failwith "unimplemented"
          (* let* _ = check_expr ctx env expected rhs in *)
          (* let* _ = expect_list () in *)
          (* _ *)
      | Concat ->
          failwith "unimplemented"
          (* let* _ = check_expr ctx env expected lhs in *)
          (* let* _ = check_expr ctx env expected rhs in *)
          (* expect_list () *))
  | Call (f, arg) -> _
  | Let (bindings, body) -> _
  | IfElse (cond, then_, else_) -> _
