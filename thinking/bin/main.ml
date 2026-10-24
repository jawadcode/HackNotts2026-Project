type term =
  | Var of string
  | Lam of string * term
  | App of term * term
  | Let of (string * term) list * term

let rec term_to_string : term -> string = function
  | Var name -> name
  | Lam (var, body) -> Printf.sprintf "\\%s. %s" var (term_to_string body)
  | App (f, arg) ->
      Printf.sprintf "(%s %s)" (term_to_string f) (term_to_string arg)
  | Let (bindings, body) ->
      let bindings_to_string bindings =
        List.fold_left
          (fun acc (var, value) ->
            acc ^ Printf.sprintf "%s = %s;" var (term_to_string value))
          "" bindings
      in
      Printf.sprintf "let %s in %s"
        (bindings_to_string bindings)
        (term_to_string body)

let lookup (env : (string * term) list) (name : string) : term =
  List.find (fun (n, v) -> n == name) env |> snd

let append env key value = (key, value) :: env

let rec eval env = function
  | (Var _ | Lam _) as term -> term
  | App (f, arg) ->
      print_endline (term_to_string @@ eval env f);
      let var, body =
        match eval env f with
        | Lam (var, body) -> (var, body)
        | _ -> failwith "not a lambda"
      in
      eval (append env var arg) body
  | Let (bindings, body) ->
      eval
        (List.map (fun (var, value) -> (var, eval env value)) bindings @ env)
        body

let () =
  let term = Let ([ ("id", Lam ("x", Var "x")) ], App (Var "id", Var "x")) in
  term_to_string term |> print_endline;
  eval [] term |> term_to_string |> print_endline
