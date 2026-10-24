open Batteries
open Parser

let test = "fun id(x : int) : int = x"
(* let test = "((((123))))" *)

let () =
  match run_parser item_parser test with
  | Ok res -> dump res |> print_endline
  | Error err -> print_endline @@ "Parser Error: " ^ err
