type opcode = 
  |NOP
  |LDN of int
  |LDB of bool
  |LOOKUP of string
  |PLUS
  |TIMES
  |AND 
  |OR 
  |NOT 
  |EQ 
  |GT 
  |COND of opcode list* opcode list
  |CASE of opcode list* opcode list 
  |CHECK of opcode list
  |PAIR
  |FST 
  |SND 
  (* |SEQ  *)
  |PAR of opcode list * opcode list
  |LOC of opcode list * opcode list
  |MKCLOS of string * opcode list
  |EXT of string * opcode list
  |RET 
  |APP
  |RED
  |TUP of int
  |PROJ of int

and value = 
  |N of int
  |B of bool
  |P of value * value
  |T of value list
  |Vclos of string * opcode list * (string * value) list
  |Edge

and exp = 
  | Num of int
  | Bl of bool
  | V of string
  | Plus of exp * exp
  | Times of exp * exp
  | And of exp * exp
  | Or of exp * exp
  | Eq of exp * exp
  | Gt of exp * exp
  | Not of exp
  | IfTE of exp * exp * exp
  | Case of exp * ((exp*exp) list)
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp
  | Abs of string * exp
  | App of exp * exp
  | Let of def * exp
  | Tuple of exp list
  | Proj of int * exp

and def = 
  | Adef of string * exp
  | Seq of def * def
  | Par of def * def
  | Loc of def * def
  
let rec find_value (key:string) (lst:(string * value) list) : value =
  match lst with
  | [] -> failwith "Key not found"
  | (k, v)::tl -> if k = key then v else find_value key tl

let rec extend lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | (k, v) :: tl ->
    try
      let _ = find_value k lst2 in
      extend tl lst2  (* Key already exists in lst2, skip this entry *)
    with
    | _ -> extend tl ((k, v) :: lst2)  (* Key doesn't exist in lst2, add it *)

let rec compile e = match e with 
  | Num n -> [LDN n]
  | Bl b -> [LDB b]
  | V x -> [LOOKUP x]
  | Plus(e1,e2) -> compile e1 @ compile e2 @ [PLUS]
  | Times(e1,e2) -> compile e1 @ compile e2 @ [TIMES]
  | And(e1,e2) -> compile e1 @ compile e2 @ [AND]
  | Or(e1,e2) -> compile e1 @ compile e2 @ [OR]
  | Not e1 -> compile e1 @ [NOT]
  | Eq(e1,e2) -> compile e1 @ compile e2 @ [EQ]
  | Gt(e1,e2) -> compile e1 @ compile e2 @ [GT]
  | IfTE(e0,e1,e2) -> (compile e0) @ [COND(compile e1, compile e2)]
  | Pair(e1,e2) -> compile e2 @ compile e1 @ [PAIR]
  | Fst(e0) -> compile e0 @ [FST]
  | Snd(e0) -> compile e0 @ [SND]
  | Abs(x,e1) -> [MKCLOS(x,compile e1 @ [RET])]
  | App(e1,e2) -> compile e1 @ compile e2 @ [APP]
  | Case(e0, lst1) -> let rec func lst = match lst with
    | [] -> [NOP]
    | (exp,sel)::tail -> [CASE(compile exp, compile sel)] @ func tail
  in compile e0 @ func lst1
  | Let(d1,e1) -> compile_def d1 @ compile e1
  | Tuple(lst) -> List.concat (List.map compile lst) @ [TUP(List.length lst)]
  | Proj(pos,tup) -> compile tup @ [PROJ pos]
  | _ -> raise Not_found

and compile_def d = match d with
  | Adef(x,e1) -> [EXT(x,compile e1)]
  | Seq(d1,d2) -> compile_def d1 @ compile_def d2
  | Par(d1,d2) -> [PAR(compile_def d1,compile_def d2)]
  | Loc(d1,d2) -> [LOC(compile_def d1,compile_def d2)]

type dump = (value list * (string * value) list * opcode list) list
exception Stuck of value list * (string * value) list * opcode list * dump

let merge lst1 lst2 =
  let rec merge_help lst1 lst2 = match (lst1, lst2) with
    | ([], lst) -> lst
    | (lst, []) -> lst
    | ((key1, value1)::tl1, (key2, value2)::tl2) ->
      if (key1 = key2 && value1 = value2) then (key1, value1) :: merge_help tl1 tl2
      else if (key1 < key2) || (key1 = key2 && value1 < value2)  then (key1, value1) :: merge_help tl1 lst2
      else (key2, value2) :: merge_help lst1 tl2
  in List.rev (merge_help (List.rev lst1) (List.rev lst2))

let remove_elements_between_delimiters lst =
  let rec aux acc = function
    | [] -> List.rev acc
    | ("$", Edge) :: rest -> 
        let rec skip_until_end_delimiter = function
          | [] -> []
          | ("$", Edge) :: t -> t
          | _ :: t -> skip_until_end_delimiter t
        in aux acc (skip_until_end_delimiter rest)
    | hd :: tl -> aux (hd :: acc) tl
  in aux [] lst

let rec secd s g c d = match s, g, c, d with 
  | v::_, g, [], _ -> (v,g)
  | s', g', LDN n :: c', d' -> secd (N n :: s') g' c' d'
  | s', g', LDB b :: c', d' -> secd (B b :: s') g' c' d'
  | s', g', LOOKUP x :: c', d' -> secd ((find_value x g) :: s') g' c' d'
  | N n1 :: N n2 :: s', g', PLUS :: c', d' -> secd (N (n1 + n2) :: s') g' c' d'
  | N n1 :: N n2 :: s', g', TIMES :: c', d' -> secd (N (n1 * n2) :: s') g' c' d'
  | B b1 :: B b2 :: s', g', AND :: c', d' -> secd (B (b1 && b2) :: s') g' c' d'
  | B b1 :: B b2 :: s', g', OR :: c', d' -> secd (B (b1 || b2) :: s') g' c' d'
  | N n1 :: N n2 :: s', g', EQ :: c', d' -> secd (B (n1 = n2) :: s') g' c' d'
  | N n1 :: N n2 :: s', g', GT :: c', d' -> secd (B (n1 > n2) :: s') g' c' d'
  | B b1 :: s', g', NOT :: c', d' -> secd (B (not b1) :: s') g' c' d'
  | B true :: s', g', COND (c1, _) :: c', d' -> secd s' g' (c1 @ c') d'
  | B false :: s', g', COND (_, c2) :: c', d' -> secd s' g' (c2 @ c') d'
  | s', g', NOP::c', d' -> secd s' g' c' d'
  | s', g', RED::CASE (exp, sel) :: c', d' -> secd s' g' ([RED]@ c') d'
  | s', g', RED::c', d' -> secd s' g' c' d'
  | s', g', CASE (exp, sel) :: c', d' -> secd s' g' (exp @ [CHECK(sel)] @ c') d' 
  | exp::e0::s', g', CHECK (sel) :: c', d' -> 
    if e0 = exp then secd s' g' (sel @ [RED] @ c') d'
    else secd (e0::s') g' c' d'
  | v1 :: v2 :: s', g', PAIR :: c', d' -> secd (P (v1, v2) :: s') g' c' d'
  | P (v1, v2) :: s', g', FST :: c', d' -> secd (v1 :: s') g' c' d'
  | P (v1, v2) :: s', g', SND :: c', d' -> secd (v2 :: s') g' c' d'
  | s', g', MKCLOS (x, c') :: c'', d' -> secd (Vclos (x, c', g') :: s') g' c'' d'
  | a :: Vclos (x, c', g'') :: s', g', APP :: c'', d -> secd [] ((x, a) :: g'') c' ((s', g', c'') :: d)
  | a :: s'', g'', RET :: c', (s', g', c'') :: d -> secd (a :: s') g' c'' d 
  | s', g', EXT(x,c')::c'', d' -> let (v,g') = (secd [] g' c' d') in secd s' ((x,v)::g') c'' d'
  | s', g', PAR(c1,c2)::c', d' ->
    let (v1, g1) = secd [N 1] g' c1 d' in
    let (v2, g2) = secd [N 1] g' c2 d' in
    let g'' = merge g1 g2 in secd s' g'' c' d'
  | s', g', LOC(c1,c2)::c', d' ->
    let (v1, g1) = secd [N 1] (("$",Edge)::g') c1 d' in
    let (v2, g2) = secd [N 1] (("$",Edge)::g1) c2 d' in 
    let g'' = remove_elements_between_delimiters g2 in secd s' g'' c' d'
  | s', g',(TUP n)::c',d' -> 
    let rec proc_tuple n acc rest= 
      if n = 0 then acc, rest
      else match rest with 
        | v::rest' -> proc_tuple (n-1) (v :: acc) rest'
        | _ -> failwith "Insufficient elements in tuple"
    in 
    let tuple_lst, s'' = proc_tuple n [] s' in
    secd (T (tuple_lst) :: s'') g' c' d'
  | T(tuple_lst) :: s', g', (PROJ pos) :: c', d' ->
    let rec get_element_at_pos n lst = match n, lst with
      | 0, hd :: _ -> hd
      | n, _ :: tl -> get_element_at_pos (n - 1) tl
      | _, [] -> failwith "Insufficient elements in tuple"
    in
    let selected_element = get_element_at_pos pos tuple_lst in
    secd (selected_element :: s') g' c' d'    
  | _, _, _, _ -> raise (Stuck (s, g, c, d))

let env = [("y",N 2);("x",N 1)]

let exp1 = Plus(Num 1, Num 2)
let c1 = compile exp1

let exp2 = Abs("x",Plus(Num 3,V "y"))
let c2 = compile exp2

let exp3 = Abs("x",Plus(Num 3,V "x"))
let c3 = compile exp3

let exp4 = Abs("y",Abs("x",Plus(V "y",V "x")))
let c4 = compile exp4

let exp5 = App(exp2,Num 4)
let c5 = compile exp5
let w5 = secd [] env c5 []

let exp6 = App(exp3,Num 4)
let c6 = compile exp6
let w6 = secd [] env c6 []

let exp7 = App((App(exp4,Num 2)),Num 4)
let c7 = compile exp7
let w7 = secd [] env c7 []

let exp8 = Abs("x",Abs("x",Abs("x",Plus(V "x",V "x"))))
let c8 = compile exp8

let exp9 = App(App((App(exp8,Num 2)),Num 4),Num 7)
let c9 = compile exp9
let w9 = secd [] env c9 []

let exp10 = Abs("x",Abs("y",Abs("x",Plus(V "y",V "x"))))
let c10 = compile exp10

let exp11 = App(App((App(exp10,Num 2)),Num 4),Num 7)
let c11 = compile exp11
let w11 = secd [] env c11 []

let exp12 = Case(
  Plus(Num 2, Num 2),
  [(Num 3, Num 10);
   (Num 4, Num 20);
   (Plus(V "x", V "y"), Plus(V "x", V "y"))]
)
let c12 = compile exp12
let w12 = secd [] env c12 []

let exp13 = Proj(2,Tuple([Num 1;Num 2;Plus(V "x",V "y")]))
let c13 = compile exp13
let w13 = secd [] env c13 []


let d1 = Adef("x",Plus(Num 2, Num 3 ))
let d2 = Adef("y",Plus(Num 2, V "x" ))
let d3 = Adef("z",Plus(Num 4, V "y" ))
let d4 = Adef("z",Plus(Num 2, V "x" ))
let d7 = Adef("x",V "y")
let d8 = Adef("y",V "x")
let d9 = Par(d7,d8)

(* Simple sequential defn *)
let l1 = Let(Seq(d1,d2),Plus(V "y",Num 1))
let c1 = compile l1
let u1 = secd [] env c1 []

(* Simple parallel defn *)
let l2 = Let(Par(d1,d2),Plus(V "y",Num 1))
let c2 = compile l2
let u2 = secd [] env c2 []

(* Parallel defn with seemingly conflicting names *)
let l7 = Let(d9,Plus(V "x", V "y"))
let c7 = compile l7
let u7 = secd [] env c7 []

(* Sequential inside parallel *)
let se = Seq(d1,d4)
let l3 = Let(Par(se,d2),Plus(V "y",V "z"))
let c3 = compile l3
let u3 = secd [] env c3 []

(* Parallel inside Sequential *)
let par = Par(d2,d3)
let l4 = Let(Seq(d1,par),Times(V "y",V "z"))
let c4 = compile l4
let u4 = secd [] env c4 [] 

(* Local vs Sequential *)
let d5 = Adef("x",Num 3)
let d6 = Adef("y",Plus(Num 2, V "x" ))

let l5 = Let(Loc(d5,d6),Plus(V "x",V "y"))
let c5 = compile l5
let u5 = secd [] env c5 [] 

let l6 = Let(Seq(d5,d6),Plus(V "x",V "y"))
let c6 = compile l6
let u6 = secd [] env c6 [] 


let t2 = secd [] [("x", N 1 )] (compile (V "x")) []
let t3 = secd [] [("x", Vclos("x", [LOOKUP "y"; RET], [("y", N 2)])); ("y", N 1 )] (compile (App(V "x", V "y"))) []
let t4 = secd [] [("x", N 1)] (compile (App(Abs("x", V "x"), V "x"))) []
let t5 = secd [] [("x", N 1); ("y", N 2)] (compile (App(Abs("y", V "y"), V "x"))) [] 
let t6 = secd [] [("x", N 1); ("y", N 2)] (compile (App(Abs("y", Abs("x", V "y")), V "x"))) []
let t7 = secd [] [("x", N 1); ("y", N 2)] (compile (App(Abs("x", App(Abs("y", V "y"), V "x")), V "x"))) []
let t1 = secd [] [("x", N 1 )] (compile (V "y")) []







