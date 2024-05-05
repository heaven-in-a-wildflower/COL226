type symbol = string * int

type tree = V of string | C of { node: symbol; children: tree list }

type substitution = (string * tree) list

let id_sub : substitution = []

let remove_variables_present_in_theta (theta:substitution) (lambda:substitution) =
  let is_variable_present_in_theta var =
    List.exists (fun (v, _) -> v = var) theta
  in
  List.filter (fun (var, _) -> not (is_variable_present_in_theta var)) lambda

let remove_null_elements =
  List.filter
    (fun (x, t) ->
      match t with
      | V v when v = x -> false (* Remove (x, V x) *)
      | _ -> true)

let rec mgu t u =
  let rec unify_children subst children1 children2 =
    match children1, children2 with
    | [], [] -> subst
    | t1 :: rest1, t2 :: rest2 ->
      let s' = mgu (subst_term subst t1) (subst_term subst t2) in
      unify_children (compose_subst s' subst) rest1 rest2
    | _ -> failwith "Different number of children"
  in
  match t, u with

  | V x, V y when x = y -> id_sub
  | V x, V y -> [(x, V y)]
  | V x, C r -> if occurs_check x u then failwith "Occurs check failed" else [(x, u)]
  | C r, V y -> if occurs_check y t then failwith "Occurs check failed" else [(y, t)]
  | C r, C r' when r.node <> r'.node -> failwith "Root symbols do not match"
  | C r, C r' -> unify_children id_sub r.children r'.children

and subst_term subst t =
  match t with
  | V x ->
    (match List.assoc_opt x subst with
    | Some s -> subst_term subst s
    | None -> V x)
  | C r -> C { node = r.node; children = List.map (subst_term subst) r.children }

and occurs_check x t =
  let rec check t =
    match t with
    | V y when x = y -> true
    | C r -> List.exists check r.children
    | _ -> false
  in
  check t

and compose_subst (theta : substitution) (lambda : substitution) : substitution =
  let apply_substitution subst tree =
    let rec replace_variables = function
      | V x -> (try List.assoc x subst with Not_found -> V x)
      | C { node; children } -> C { node; children = List.map replace_variables children }
    in
    replace_variables tree
  in
  let theta_lambda = List.map (fun (x, t) -> (x, apply_substitution lambda t)) theta in
  let clean_lambda = remove_variables_present_in_theta theta lambda in 
  let theta_lambda_all = theta_lambda @ clean_lambda in
  let theta_lambda_filtered = remove_null_elements theta_lambda_all in
  (* List.iter (fun (v, t) -> Printf.printf "(%s, %s)\n" v (tree_to_string t)) theta_lambda_all; *)
  theta_lambda_filtered

(* Example usage *)
(* let t = C { node = ("f", 2); children = [C { node = ("h", 2); children = [C { node = ("a", 0); children = [] }; V "X"] }; C { node = ("h", 2); children = [V "X"; C { node = ("b", 0); children = [] }] }] };;
let u = C { node = ("f", 2); children = [C { node = ("h", 2); children = [C { node = ("a", 0); children = [] }; C { node = ("b", 0); children = [] }] }; C { node = ("h", 2); children = [V "Z"; V "X"] }] } *)

(* let t = C { node = ("Q", 2); children = [C { node = ("f", 1);children = [C { node = ("a", 0);children=[]}]};C { node = ("g", 1);children = [V "x"]}]}
let u = C { node = ("Q", 2); children = [V "y";V "z"]} *)

(* let t = C { node = ("Q", 2); children = [C { node = ("f", 1);children = [C { node = ("a", 0);children=[]}]};V "x"]}
let u = C { node = ("Q", 2); children = [V "y";V "y"]} *)

(* let t = C { node = ("f", 1); children = [V "x"] }
let u = C { node = ("g", 1); children = [V "y"] } *)

(* let t = C { node = ("f", 2); children = [V "x"; V "z"] }
let u = C { node = ("f", 2); children = [V "x"; C { node = ("g", 1); children = [V "y"] }] } *)

(* let t = C { node = ("f", 1); children = [V "x"] }
let u = C { node = ("f", 1); children = [C { node = ("g", 1); children = [V "x"] }] } *)

(* let t = C { node = ("f", 2); children = [V "x";C {node = ("g", 2); children = [C {node = ("a", 0); children = []};C {node = ("c", 0); children = []}]}]}

let u = C { node = ("f", 2); children = [C { node = ("a", 0); children = []};C { node = ("g", 2); children = [V "x"; C { node = ("c", 0); children = []}]}]} *)

(* let t = C {
  node = ("f", 2);
  children = [V "x"; V "y"]
}

let u = C {
  node = ("f", 2);
  children = [V "y"; V "x"]
} *)

(* let t = C {node = ("*",2);children = [C{node = ("+",2);children = [V"x";V"y"]};V"x"]}

let u = C {node = ("*",2);children = [C{node = ("+",2);children = [V"a";V"b"]};C{node = ("-",1);children = [V"b"]}]}  *)

(* C{node = ("-",1);children = [C { node = ("a", 0); children = []}]}
C{node = ("+",2);children = [C { node = ("a", 0); children = []};C { node = ("b", 0); children = []}]} *)

(* let mgu_result1 = mgu t u
let mgu_result2 = mgu u t
let rec string_of_tree t =
  match t with
  | V v -> v
  | C {node:symbol; children} ->
    let children_str = List.map string_of_tree children |> String.concat ", " in
    "(" ^ fst node ^ "[" ^ children_str ^ "])"

let string_of_subst subst =
  List.map (fun (x, t) -> x ^ " -> " ^ (string_of_tree t)) subst
  |> String.concat ", "

let () =
  match mgu_result1 with
  | exception Failure msg -> print_endline ("Failed: " ^ msg)
  | subst -> print_endline ("Most General Unifier 1: {" ^ (string_of_subst subst) ^ "}")
let()=  
  match mgu_result2 with
  | exception Failure msg -> print_endline ("Failed: " ^ msg)
  | subst -> print_endline ("Most General Unifier 2: {" ^ (string_of_subst subst) ^ "}") *)
