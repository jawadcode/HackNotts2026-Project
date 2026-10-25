open Batteries
open Parser
open Ast

(* let test = *)
(*   "\n\ *)
(*   \      fun id(x : Int, y : Bool, z : String) : Int =\n\ *)
(*   \       let id : Fun of int -> int = fun (x : int) : int -> x in\n\ *)
(*   \         if (x == (id x)) and y\n\ *)
(*   \           then 123\n\ *)
(*   \           else 234\n" *)

let test =
  "\n\
   fun id(x : Int, y : Bool, z : String) : Int =\n\
  \  let id : Int = 123 in id\n"

(* let test = "((((123))))" *)

(* let test = "\nfun f(x : Int, y : Bool, z : String) : Int = 123\n" *)

let () =
  print_endline ("\"\"\"" ^ test ^ "\"\"\"");
  match run_parser item_parser test with
  | Ok res -> string_of_item res |> print_endline
  | Error err -> print_endline @@ "Parser Error: " ^ err
