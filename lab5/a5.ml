open List
open Seq
module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

type symbol = string * int
type signature = symbol list

(*1.Write a program to check that a purported signature is indeed legitimate, i.e.,
that no symbol appears twice, and that the arities are all non-negative.*)

(* let rec has_duplicate_symbol = function
  | [] -> false
  | (name, _) :: tl -> List.exists (fun (n, _) -> n = name) tl || has_duplicate_symbol tl

let all_non_negative_arities sig_list =
  List.for_all (fun (_, arity) -> arity >= 0) sig_list

let check_sig signature =
  not (has_duplicate_symbol signature) && all_non_negative_arities signature

(* Test cases *)

let sig1 = [("0", 0); ("1", 0); ("0", 1)]
let sig2 = [("0", 0); ("1", 0); ("+", 2)]
let signature1 = [("a", 2); ("b", 1); ("c", 0)]
let signature2 = [("a", 2); ("b", 1); ("a", 0)]
let signature3 = [("a", 2); ("b", -1); ("a", 0)]

let () =
  print_endline (string_of_bool (check_sig sig1)); 
  print_endline (string_of_bool (check_sig sig2)); 
  print_endline (string_of_bool (check_sig signature1)); 
  print_endline (string_of_bool (check_sig signature2));
  print_endline (string_of_bool (check_sig signature3));  *)

(***********************************************************************************************************)

(* 2. Given a valid signature (checked using check_sig), define a function wftree that checks that a given tree (pre-term) is well-formed according to the signature.*)

type tree = V of string
| C of {node: symbol; children: tree list}

let rec wftree (sign : signature) (t : tree) : bool =
  let rec arity_of_symbol (sym : symbol) : int =
    match List.assoc_opt (fst sym) sign with
    | Some arity -> arity
    | None -> failwith ("Symbol " ^ fst sym ^ " not found in signature")
  in
  match t with
  | V _ -> true (* Variables are always well-formed *)
  | C {node; children} ->
    let arity = arity_of_symbol node in
    let node_arity_matches = (arity = snd node) in
    node_arity_matches &&
    let child_arity_ok =
      List.for_all
        (fun child ->
          match child with
          | V _ -> true (* Variables are always well-formed *)
          | C {node=child_node; children=grandchildren} -> arity_of_symbol child_node = List.length grandchildren
        )
        children
    in
    node_arity_matches && child_arity_ok && List.for_all (wftree sign) children

(* Example signature and tree *)
(* let signature1 : signature = [("f", 2); ("g", 2); ("a", 0); ("b", 0)]

let example_tree1: tree =
  C {
    node = ("f", 2);
    children = [
      C {node = ("g", 2); children = [V "x"; V "y"]};
      C {node = ("g", 2); children = [C{node=("a",0);children=[]};C{node=("b",0);children=[]}]}
    ]
  }
  
let example_tree2: tree =
C {
  node = ("f", 2);
  children = [
    C {node = ("g", 2); children = [V "x"; V "y"]};
    C {node = ("g", 1); children = [C{node=("a",0);children=[]};C{node=("b",0);children=[]}]}
  ]
}

let example_tree3: tree =
C {
  node = ("f", 2);
  children = [
    C {node = ("g", 2); children = [V "x"; V "y"]};
    C {node = ("g", 2); children = [C{node=("a",0);children=[]}]}
  ]
} 

let sig1 = [("0", 0); ("1", 0); ("0", 1)];;
let sig2 = [("0", 0); ("1", 0); ("+", 2)];;
let t = C {node = ("+", 2); children = [(V "x"); (V "y"); (V "z")]} ;;
let t2 = C {node = ("+", 2); children = [(V "x"); (V "y")]} ;;
let t3 = C {node = ("+", 2); children = [(V "z"); t2]} ;;

let()=
  print_endline(string_of_bool(wftree signature1 example_tree1));
  print_endline(string_of_bool(wftree signature1 example_tree2));
  print_endline(string_of_bool(wftree signature1 example_tree3));
  print_endline(string_of_bool(wftree sig2 t)) *)

(***********************************************************************************************)
(* 3. Define functions ht, size and vars that given a well-formed tree, return its height, its size and the set of variables appearing in it respectively.  Use map, fold_left and other such functions as far as possible wherever you use lists.  *)

(* let rec ht t =
match t with
| V _ -> 0
| C r ->
  let (s, n) = r.node in
  if n = 0 then 0
  else 1 + (List.fold_left max 0 (List.map ht r.children))

let rec size t =
  match t with
  | V _ -> 1
  | C r -> 1 + (List.fold_left (+) 0 (List.map size r.children))

let rec vars t =
  let rec include_vars acc t =
    match t with
    | V v -> StringSet.add v acc
    | C {node; children} -> List.fold_left include_vars acc children
  in
include_vars StringSet.empty t

let example_tree1: tree =
  C {
    node = ("f", 2);
    children = [
      C {node = ("g", 2); children = [V "x"; C{node=("b",0);children=[]}]};
      C {node = ("g", 2); children = [C{node=("a",0);children=[]}; V "y"]}
    ]
  }

let t2 = C {node = ("+", 2); children = [(V "x"); (V "y")]} ;;

let() = 
  print_endline(string_of_int(size example_tree1));
  print_endline(string_of_int(ht example_tree1));
  let vars_string = String.concat ", " (StringSet.elements (vars example_tree1)) in
  print_endline vars_string;
  print_endline(string_of_int(size t2));
  print_endline(string_of_int(ht t2)) *)
  
(********************************************************************************************)

(*4. Write a function mirror, which given a tree t returns a tree that is the mirror image of t. That is, at each level for each node, its children are reversed. *)
(* let rec mirror t =
  match t with
  | V v -> V v 
  | C {node; children} ->
    let mirrored_children = List.map mirror (List.rev children) in
    C {node; children = mirrored_children}
  
let rec string_of_tree t =
  match t with
  | V v -> v
  | C {node:symbol; children} ->
    let children_str = List.map string_of_tree children |> String.concat ", " in
    "(" ^ fst node ^ "[" ^ children_str ^ "])"

let example_tree : tree =
  C {
    node = ("f", 2);
    children = [
      C {node = ("g", 2); children = [V "x"; V "y"]};
      C {node = ("h", 2); children = [C{node=("a",0);children=[]};C{node=("b",0);children=[]}]}
    ]
  }

let mirror_tree = mirror example_tree
let t2 = C {node = ("+", 2); children = [(V "x"); (V "y")]} ;;
let t3 = C {node = ("+", 2); children = [(V "z"); t2]} ;;
let mirror_t3 = mirror t3

let () =
  let example_tree_str = string_of_tree example_tree in
  let mirror_tree_str = string_of_tree mirror_tree in
  print_endline "Original Tree:";
  print_endline example_tree_str;
  print_endline "Mirror Tree:";
  print_endline mirror_tree_str
let() =
  let t3_str = string_of_tree example_tree in
  let mirror_t3_str = string_of_tree mirror_tree in
  print_endline "Original Tree:";
  print_endline t3_str;
  print_endline "Mirror Tree:";
  print_endline mirror_t3_str *)

(********************************************************************************************)
(*5. Define a suitable representation for substitutions as a table defined as a list of pairs (check that the table is a valid representation of a function).  Come up with an efficient representation of composition of substitutions. *)

type substitution = (string * tree) list

let check_valid_substitution (subst : substitution) : bool =
  let seen = ref [] in
  List.for_all (fun (var, t) ->
    if List.mem var !seen then false
    else (seen := var :: !seen; true)
  ) subst

let rec substitute_tree (subst : substitution) (tree : tree) : tree =
  match tree with
  | V var ->
    begin
      match List.assoc_opt var subst with
      | Some t -> t
      | None -> V var
    end
  | C {node; children} ->
    let new_children = List.map (substitute_tree subst) children in
    C {node; children = new_children}

(* let subst_table = [("x", C {node=("f", 2); children=[V "x"; V "z"]} );("x",V "z" )]

let eg_tree = C {node=("g", 2); children=[V "x"; V "y"]}

let substituted_tree = substitute_tree subst_table eg_tree

let() = 
    print_endline(string_of_bool(check_valid_substitution subst_table));
    print_endline(string_of_tree(eg_tree));
    print_endline(string_of_tree(substituted_tree)) *)
  
let rec subst sigma t = match t with
  | V x -> sigma x
  | C r -> C { node = r.node; children = List.map (subst sigma) r.children }
  
let compose_subst s1 s2 t = subst s2 (subst s1 t);;

let subst1 x = if x = "x" then C {node=("g", 2); children=[V "y"]}  else if x = "y" then V "z" else V x
let subst2 x = if x = "x" then C {node=("a", 0); children=[]} else if x = "y" then C {node=("b", 0); children=[]} else if x = "z" then V "y" else V x
let composed_subst = compose_subst subst1 subst2

let get_string_from_var var =
  match var with
  | V str -> str
  | _ -> failwith "Expected a V constructor"

let print_composed_subst () =
let variables = [V "x"; V "y"; V "z"] in
List.iter (fun var ->
  let result = composed_subst var in
  if result <> V (get_string_from_var var) then
    Printf.printf "%s -> %s\n" (get_string_from_var var) (string_of_tree result)
) variables
  
(* let()=
    print_composed_subst() *)



