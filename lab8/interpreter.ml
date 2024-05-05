type variable = string
type symbol = string
type signature = (symbol * int) list
type term = Var of variable | Tree_node of symbol * (term list)
type atom = Atom of symbol * (term list)
type head = Head of atom
type body = Body of atom list
type clause = Fact of head | Rule of head * body
type program = Prog of clause list
type goal = Goal of atom list
type subst = (variable * term) list

exception No_unifier_exists
exception Variable_absent
exception InvalidProgram
exception Invalid_list

let rec union lst1 lst2 = 
  let rec exists elem lst = List.exists (fun x -> x = elem) lst
in match lst1 with 
  | [] -> lst2
  | x::xs -> if (exists x lst2) then (union xs lst2) else x::(union xs lst2)

let rec rename_program (Atom(s, _): atom) (prog:program): clause list =
  let rec rename_clause (cl:clause): clause = 
    let rec rename_atom (a:atom): atom = 
      let rec rename_term (t:term): term = match t with
        Var(v) -> Var("$" ^ v)
      | Tree_node(s, l) -> Tree_node(s, List.map (rename_term) l)
      | _ -> t
    in match a with
  Atom(s, l) -> Atom(s, List.map (rename_term) l)
  in match cl with
    Fact(Head(a)) -> Fact(Head(rename_atom a))
  | Rule(Head(a), Body(l)) -> Rule(Head(rename_atom a), Body(List.map (rename_atom) l))
in match prog with
  | Prog [] -> []
  | Prog (cl::ps) -> match cl with Fact(Head(Atom(s', _))) | Rule(Head(Atom(s', _)), _) ->
                if s = s' then (rename_clause cl)::(rename_program (Atom(s, [])) (Prog ps) )
                else cl::(rename_program (Atom(s, [])) (Prog ps))
                
let vars_in_goal (Goal(gs) : goal) = 
  let vars_in_atom (Atom(s,lst) : atom) = 
    let rec vars_in_term (t:term) = match t with 
    | Var x -> [x]
    | Tree_node(s,lst) -> List.fold_left union [] (List.map vars_in_term lst)  
  in vars_in_term (Tree_node(s,lst))
in List.fold_left union [] (List.map vars_in_atom gs)

let remove_variables_present_in_theta (theta:subst) (lambda:subst) =
  let is_variable_present_in_theta var =
    List.exists (fun (v, _) -> v = var) theta
  in
  List.filter (fun (var, _) -> not (is_variable_present_in_theta var)) lambda

let remove_null_elements = List.filter (fun (x, t) -> match t with
                            | Var v when v = x -> false (* Remove (x, V x) *)
                            | _ -> true)

let rec apply_sub (sub: subst) (t: term) : term = match t with
| Var x -> (match List.assoc_opt x sub with
          | Some u -> apply_sub sub u
          | None -> Var x)
| Tree_node(s,lst) -> Tree_node(s, List.map (fun t' -> apply_sub sub t') lst)
      
let compose_sub (theta:subst) (lambda : subst) : subst = 
  let l1 = List.map (fun(x,t) -> (x, apply_sub lambda t)) theta in 
    let l2 = remove_variables_present_in_theta theta lambda in 
      let l3 = l1@l2 in 
        let composed_lst = remove_null_elements l3 in composed_lst

let occurs (x:string) (t:term) : bool = 
  let rec check t = match t with 
  | Var y -> if (x=y) then true else false
  | Tree_node(s,lst) -> List.exists check lst
in check t

let rec mgu (t : term) (u : term) : subst = 
  let id_sub = [] in 
  let rec unify_children (sub:subst) (children1 : term list) (children2 : term list) = 
    match children1, children2 with 
    | [],[] -> sub
    | t1::rem_ts, u1::rem_us -> let sub' = mgu (apply_sub sub t1) (apply_sub sub u1) in unify_children (compose_sub sub sub') rem_ts rem_us
    | _ -> failwith "Different number of children"
  in match t, u with 
    | Var x, Var y -> if (x=y) then [] else [(x,Var y)]
    | Var x, Tree_node(_,_) -> if occurs x u then raise No_unifier_exists else [(x,u)]
    | Tree_node(_,_), Var y -> if occurs y t then raise No_unifier_exists else [(y,t)]
    | Tree_node(s1,lst1), Tree_node(s2,lst2) -> if (s1<>s2) then raise No_unifier_exists else 
                                        unify_children id_sub lst1 lst2 

let rec format_term_list (tl:term list) = match tl with
    [] -> Printf.printf ""
  | [t] -> format_term t
  | t::tls -> (
      format_term t;
      Printf.printf ",";
      format_term_list tls;
    )
and format_term (t:term) = match t with
| Var(v) -> Printf.printf " %s " v
| Tree_node(sym, []) -> Printf.printf " %s " sym
| Tree_node(sym, l) -> (
    Printf.printf " %s ( " sym;
    format_term_list l;
    Printf.printf " ) ";
  )

let rec reconstruct_solution (theta: subst) (vars: variable list) =
  match vars with
  | [] -> []
  | v :: vs ->
    (try
      List.find (fun (x, _) -> x = v) theta :: reconstruct_solution theta vs
    with Not_found -> reconstruct_solution theta vs)

let rec format_solution (theta:subst) = match theta with
  [] -> Printf.printf "true. "
| [(v, t)] -> (
    Printf.printf "%s =" v;
    format_term t;
  )
| (v, t)::xs -> (
    Printf.printf "%s =" v;
    format_term t;
    Printf.printf ", ";
    format_solution xs;
  )
let accept_char () =
  let input_output = Unix.tcgetattr Unix.stdin in
  let () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
          { input_output with Unix.c_icanon = false } in
  let result = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN input_output;
  result

let my_swipl (g':goal) (p':program) = 
  let rec handle_goal (g:goal) (p:program) (theta:subst) (vars:variable list): (subst * bool) =
    match g with
      | Goal [] ->
        let solution = reconstruct_solution theta vars in
        format_solution solution;
        flush stdout;
        let rec loop () =
          Printf.printf "Action? ";
          flush stdout;
          match accept_char () with
          | '.' -> ([], true)
          | ';' -> ([], false)
          | c ->
             Printf.printf "\nUnknown Action: %c\n" c;
             loop ()
        in
        loop ()
      | Goal(a::gs) -> match a with
          | Atom("&cut", _) -> let _ = handle_goal (Goal(gs)) p theta vars in ([],true)
          | _ ->
            let new_program = Prog (rename_program a p) in
            let rec iter (program':program) = 
              let unify_atoms (Atom(s1,lst1):atom) (Atom(s2,lst2):atom) (theta:subst): subst =
                compose_sub theta (mgu (apply_sub theta (Tree_node(s1,lst1))) (apply_sub theta (Tree_node(s2,lst2))))
            in match program' with
              | Prog [] -> ([],false)
              | Prog(cl::ps) -> match cl with
                  Fact(Head(a')) -> (
                    try
                      let u = (unify_atoms a' a theta) in
                      match (handle_goal (Goal(gs)) new_program  u vars) with
                          (u',true) -> (u',true)
                        | _ -> iter (Prog ps)
                    with No_unifier_exists -> iter (Prog ps)
                  )
                | Rule(Head(a'), Body(al)) -> (
                    try
                      let u = (unify_atoms a' a theta) in
                      match (handle_goal (Goal(al @ gs)) new_program u vars) with
                          (u',true) -> (u',true)
                        | _ -> iter (Prog ps)
                    with No_unifier_exists -> iter (Prog ps)
                  )
          in iter p
in handle_goal g' p' [] (vars_in_goal g')

