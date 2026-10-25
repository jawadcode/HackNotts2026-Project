open Batteries
open Parser
open Ast

let test = "fun id(x : Int, y : Bool, z : String) : Int = x"
(* let test = "((((123))))" *)

let () =
  print_endline (dump test);
  match run_parser item_parser test with
  | Ok res -> string_of_item res |> print_endline
  | Error err -> print_endline @@ "Parser Error: " ^ err
