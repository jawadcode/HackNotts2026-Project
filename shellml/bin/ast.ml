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
  | List of expr
  | BinOp of expr * bop * expr
  | Call of expr * expr
  | Let of (string * ty * expr) list * expr
  | IfElse of expr * expr * expr

and ty = UnitTy | IntTy | StringTy | ListTy of ty | FunTy of ty * ty
and bop = Add | Sub | Mul | Div
