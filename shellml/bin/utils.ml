open Batteries

(** Produces a list of results of iteratively unconsing [list] *)
let rec unconsing list =
  match list with [] -> [] | head :: tail -> tail :: unconsing tail

(** Tries to map a [list] with a fallible function [f], short-circuiting on an
    error *)
let rec try_map f list =
  let open Result.Syntax in
  match list with
  | [] -> Ok []
  | head :: tail -> (
      match f head with
      | Ok res ->
          let+ tail = try_map f tail in
          res :: tail
      | Error err as res -> res)
