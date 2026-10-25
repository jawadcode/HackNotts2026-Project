open Batteries
open Ast
open Result.Syntax
open Utils

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
  | TEUnmetConstraint of { constr : string; got : typ }

let lookup var = List.find_opt (fun (name, ty) -> var = name) %> Option.map snd
let insert var ty = ( :: ) (var, ty)

let rec check_item env (ItemFun { name; param; ret_ty; body }) =
  let env = param :: env |> List.map (Tuple2.map2 typ_of_ty) in
  check_expr name env (typ_of_ty ret_ty) body

(** `ctx` is an attempt to salvage some meaningful location info *)
and check_expr ctx env expected expr =
  let* inferred = infer_expr ctx env (* expected *) expr in
  if inferred = expected then Ok ()
  else Error (ctx, TEMismatch { expected; inferred })

and infer_expr ctx env (* expected *) expr =
  match expr with
  | Unit -> Ok TUnit
  | IntLit _ -> Ok TInt
  | StringLit s -> Ok TString
  | Ident name ->
      lookup name env |> Stdlib.Option.to_result ~none:(ctx, TEUnbound name)
  | List elems -> (
      match elems with
      | [] -> Ok TListOfT
      | head :: tail -> (
          let* head_ty = infer_expr ctx env (* expected *) head in
          List.find_map_opt
            (fun elem ->
              match check_expr ctx env head_ty elem with
              | Ok () -> None
              | Error error -> Some error)
            tail
          |> function
          | Some (ctx, err) -> Error (ctx, err)
          | None -> Ok (TList head_ty)))
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
      | Eq ->
          let* left = infer_expr ctx env lhs in
          let* left =
            match left with
            | (TBool | TInt | TString | TList _) as ty -> Ok ty
            | ty ->
                Error
                  (ctx, TEUnmetConstraint { constr = "comparable"; got = ty })
          in
          let+ _ = check_expr ctx env left rhs in
          TBool
      | Less | Greater ->
          let* left = check_expr ctx env TInt lhs in
          let+ right = check_expr ctx env TInt rhs in
          TInt
      | Cons ->
          failwith "unimplemented"
          (* let* _ = check_expr ctx env expected rhs in *)
          (* let* _ = expect_list () in *)
          (* _ *)
      | Concat ->
          let* left = infer_expr ctx env lhs in
          let* left =
            match left with
            | (TList _ | TListOfT) as ty -> Ok ty
            | ty ->
                Error (ctx, TEMismatch { expected = TListOfT; inferred = ty })
          in
          let+ _ = check_expr ctx env left rhs in
          left)
  | Lam { param; param_ty; ret_ty; body } ->
      let param_ty = typ_of_ty param_ty and ret_ty = typ_of_ty ret_ty in
      let env = (param, param_ty) :: env in
      let+ _ = check_expr ctx env ret_ty body in
      TFun (param_ty, ret_ty)
  | Call (f, arg) ->
      let* fun_ty = infer_expr ctx env f in
      let* in_ty, out_ty =
        match fun_ty with
        | TFun (in_ty, out_ty) -> Ok (in_ty, out_ty)
        | ty -> Error (ctx, TEUnmetConstraint { constr = "callable"; got = ty })
      in
      let+ _ = check_expr ctx env in_ty arg in
      out_ty
  | Let (bindings, body) ->
      let* env =
        let+ binds =
          try_map
            (fun (name, ty, value) ->
              let ty = typ_of_ty ty in
              let+ _ = check_expr ctx env ty value in
              (name, ty))
            bindings
        in
        binds @ env
      in
      infer_expr ctx env body
  | IfElse (cond, then_, else_) ->
      let* _ = check_expr ctx env TBool cond in
      let* then_ = infer_expr ctx env then_ in
      let+ else_ = check_expr ctx env then_ else_ in
      then_
