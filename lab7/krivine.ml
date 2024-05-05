(* Krivine Machine *)

exception Error
type ans = 
  |N of int
  |B of bool
  |P of ans * ans
  |T of ans list
  |NIL
  |Vclos of string * ans
  |Bound of string
  (* The below are essentially opcodes and come into play only in value closures *)
  |PLUS of ans * ans
  |TIMES of ans * ans
  |DIV of ans * ans
  |AND of ans * ans
  |OR of ans * ans
  |EQ of ans * ans
  |GT of ans * ans
  |NOT of ans
  |IFTE of ans * ans * ans
  |PROJ of int * ans
  |TUPLE of ans list

and exp = 
  | Num of int
  | Bl of bool
  | V of string
  | Plus of exp * exp
  | Times of exp * exp
  | Div of exp * exp
  | And of exp * exp
  | Or of exp * exp
  | Eq of exp * exp
  | Gt of exp * exp
  | Not of exp
  | IfTE of exp * exp * exp
  | Path of exp * exp  (*Helper for IfTE*)
  | Abs of string * exp
  | App of exp * exp
  | Let of def * exp
  | Tuple of exp list
  | Box of exp list * ans list (*Helper for Tuple*)
  | Proj of int * exp
  | Nil (* Helper to make the evaluation tail recursive *)

and def = 
  | Adef of string * exp
  | Seq of def * def
  | Par of def * def
  | Loc of def * def

and clos = Clos of (exp * ((string * clos) list))

let rec find_in_table x g = match g with
    |[] -> failwith "Not found"
    |(x1,x2)::tl -> if(x1=x) then x2 else find_in_table x tl

let rec extend lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | (k, v) :: tl ->
    try
      let _ = find_in_table k lst2 in
      extend tl lst2  (* Key already exists in lst2, skip this entry *)
    with
    | _ -> extend tl ((k, v) :: lst2)  (* Key doesn't exist in lst2, add it *)

let rec elab d g = match d with 
  | Adef(x,e) -> [(x,Clos(e,g))]
  | Seq(d1,d2) -> let g1 = (elab d1 g) in let g2 = (elab d2 (extend g g1)) in (extend g1 g2)
  | Par(d1,d2) -> let g1 = (elab d1 g) and g2 = (elab d2 g) in (extend g1 g2)
  | Loc(d1,d2) -> let g1 = (elab d1 g) in let g2 = (elab d2 (extend g g1)) in g2

(* f is the focus closure, s is the stack *)
(* When the expression in focus closure no longer has a variable which may be substituted using the table, computation is halted *)

let rec gen_unpack bound_vars cl = match cl with
  |Clos(Num n,g) -> N n
  |Clos(Bl b,g) -> B b
  |Clos(Nil,g) -> NIL
  |Clos (V x, g) ->
    if List.mem x bound_vars then Bound x
    else gen_unpack bound_vars (find_in_table x g)
  | Clos (Plus (e1, e2), g) -> PLUS (gen_unpack bound_vars (Clos (e1, g)), gen_unpack bound_vars (Clos (e2, g)))
  | Clos (Times (e1, e2), g) -> TIMES (gen_unpack bound_vars (Clos (e1, g)), gen_unpack bound_vars (Clos (e2, g)))
  |Clos (Div (e1, e2), g) -> DIV (gen_unpack bound_vars (Clos (e1, g)), gen_unpack bound_vars (Clos (e2, g)))
  |Clos (And (e1, e2), g) -> AND (gen_unpack bound_vars (Clos (e1, g)), gen_unpack bound_vars (Clos (e2, g)))
  |Clos (Or (e1, e2), g) -> OR (gen_unpack bound_vars (Clos (e1, g)), gen_unpack bound_vars (Clos (e2, g)))
  |Clos (Not e1, g) -> NOT (gen_unpack bound_vars (Clos (e1, g)))
  |Clos (Gt (e1, e2), g) -> GT (gen_unpack bound_vars (Clos (e1, g)), gen_unpack bound_vars (Clos (e2, g)))
  |Clos (Eq (e1, e2), g) -> EQ (gen_unpack bound_vars (Clos (e1, g)), gen_unpack bound_vars (Clos (e2, g)))
  |Clos (IfTE(e0,e1,e2),g) -> IFTE (gen_unpack bound_vars (Clos (e0, g)), gen_unpack bound_vars (Clos (e1, g)),gen_unpack bound_vars (Clos (e2, g)))
  |Clos (Tuple(exp_lst),g) -> let rec map f l1 l2 = match l2 with
    |[] -> []
    |x::xs -> (f l1 (Clos(x,g)))::(map f l1 xs) in
    TUPLE(map gen_unpack bound_vars exp_lst)
  |Clos(Proj(i,e),g) -> PROJ(i,gen_unpack bound_vars (Clos(e,g))) 

  (* Clearly formal parameter x couldn't be bound with a value,  *)
  (* Consider only the outermost formal paramter *)
  (* what we want is assignment of free variables from the existing table *)
  (* Scope of this bindinig occurence of x is throughout the expression e unless there is a hole(another binding occurence of x in e. However, this can be dealt with recursively. *)
  (* But what if a different variable say y is bound to a closure with an expression containing x with a table g'. If g'= g, then we will have to consider x and it cannot be removed from the table(note that in such a case, y cannot be present in the expression, otheriwse that would lead to an infinite loop) *)
  (* Conclusion : Replace all variables except x in the expression e using the table g *)
  (* We will consider only those free variables which are outside the scope of another Abs expression if any, in the expression e. *)
  (* This is essentially unpacking the expression e using the table g and skipping(but not deleting) the entry corresponding to x. *)
  (* What about expressions like \x.((\y.3)(w)) ? => we would also need a method to unpack App with undefined expressions *)
  (* We send the App expression back to krivine machine for processing. *)
  |Clos(Abs(x,e),g) -> Vclos(x, gen_unpack (x::bound_vars) (Clos(e,g)))
  |Clos(App(e1,e2),g) -> krivine (Clos(App(e1,e2),g)) []

and unpack cl = gen_unpack [] cl

and krivine f s = match f , s with  
  | Clos(Plus(e1,e2),g), s -> krivine (Clos(e1,g)) (Clos(Plus(Nil,e2),g)::s)
  | Clos(Num(n),g), (Clos(Plus(Nil,e2),g')::s) -> krivine (Clos(e2,g')) (Clos(Plus(Num(n),Nil),g')::s)
  | Clos(Num(n2),g), (Clos(Plus(Num(n1),Nil),g')::s) -> krivine (Clos(Num(n1+n2),g')) s

  | Clos(Times(e1,e2),g), s -> krivine (Clos(e1,g)) (Clos(Times(Nil,e2),g)::s)
  | Clos(Num(n),g), (Clos(Times(Nil,e2),g')::s) -> krivine (Clos(e2,g')) (Clos(Times(Num(n),Nil),g')::s)
  | Clos(Num(n2),g), (Clos(Times(Num(n1),Nil),g')::s) -> krivine (Clos(Num(n1*n2),g')) s

  | Clos(Div(e1,e2),g), s -> krivine (Clos(e1,g)) (Clos(Div(Nil,e2),g)::s)
  | Clos(Num(n),g), (Clos(Div(Nil,e2),g')::s) -> krivine (Clos(e2,g')) (Clos(Div(Num(n),Nil),g')::s)
  | Clos(Num(n2),g), (Clos(Div(Num(n1),Nil),g')::s) -> krivine (Clos(Num(n1/n2),g')) s

  | Clos(And(e1,e2),g), s -> krivine (Clos(e1,g)) (Clos(And(Nil,e2),g)::s)
  | Clos(Bl(b),g), (Clos(And(Nil,e2),g')::s) -> krivine (Clos(e2,g')) (Clos(And(Bl(b),Nil),g')::s)
  | Clos(Bl(b2),g), (Clos(And(Bl(b1),Nil),g')::s) -> krivine (Clos(Bl(b1 && b2),g')) s

  | Clos(Or(e1,e2),g), s -> krivine (Clos(e1,g)) (Clos(Or(Nil,e2),g)::s)
  | Clos(Bl(b),g), (Clos(Or(Nil,e2),g')::s) -> krivine (Clos(e2,g')) (Clos(Or(Bl(b),Nil),g')::s)
  | Clos(Bl(b2),g), (Clos(Or(Bl(b1),Nil),g')::s) -> krivine (Clos(Bl(b1 || b2),g')) s 

  | Clos(Eq(e1,e2),g), s -> krivine (Clos(e1,g)) (Clos(Eq(Nil,e2),g)::s)
  | Clos(Num(n),g), (Clos(Eq(Nil,e2),g')::s) -> krivine (Clos(e2,g')) (Clos(Eq(Num(n),Nil),g')::s)
  | Clos(Num(n1),g), (Clos(Eq(Num(n2),Nil),g')::s) -> krivine (Clos(Bl(n1=n2),g')) s

  | Clos(Gt(e1,e2),g), s -> krivine (Clos(e1,g)) (Clos(Gt(Nil,e2),g)::s)
  | Clos(Num(n),g), (Clos(Gt(Nil,e2),g')::s) -> krivine (Clos(e2,g')) (Clos(Gt(Num(n),Nil),g')::s)
  | Clos(Num(n1),g), (Clos(Gt(Num(n2),Nil),g')::s) -> krivine (Clos(Bl(n1>n2),g')) s

  | Clos(Tuple(e::es),g),s -> krivine (Clos(e,g)) (Clos(Box(es,[]),g)::s)
  | Clos(a,g), (Clos(Box(e::es,al),g')::s) -> krivine (Clos(e,g')) (Clos(Box(es, al@[unpack(Clos(a,g))]),g')::s)
  | Clos(a,g), (Clos(Box([],al),g')::s) -> T(al@[unpack (Clos(a,g))])
  
  | Clos(Proj(i,Tuple(e)),g),s -> let rec iterate j lst = match j, lst with
    | 0, (x::xs) -> x
    | j, (x::xs) -> iterate (j-1) xs
    | _ -> failwith "Not enough elements in tuple"
  in krivine (Clos(iterate i e,g)) s

  | Clos(IfTE(e0,e1,e2),g),s -> krivine (Clos(e0,g)) (Clos(Path(e1,e2),g)::s)
  | Clos(Bl b,g), (Clos(Path(e1,e2),g')::s) -> if(b=true) then krivine (Clos(e1,g')) s else krivine (Clos(e2,g')) s 

  |	Clos(V x,g), s -> let rec find a s = match s with
  							[] -> raise Error
  						  |	(x1, x2)::xs -> if (x1=a) then x2 else find a xs in
  						krivine (find x g) s
  | Clos(Let(d,e),g), s -> let g' = (elab d g) in krivine (Clos(e,(extend g g'))) s

  | Clos(Abs(x,e),g), cl::s -> krivine  (Clos(e, (x,cl)::g)) s

  | Clos(App(e1, e2), g), s -> krivine (Clos(e1, g)) ((Clos(e2, g))::s)
  
  (* This case must be at the end *)
  | Clos(e,g), [] -> unpack (Clos(e,g))

let exec e = krivine (Clos(e,[])) []

let env = [("z",Clos(Num 3,[]));("y",Clos(Num 2,[]));("x",Clos(Num 1,[]))]

(* Value closures *)
let e = Abs("x",Plus(V "x", Num 2))
let a = krivine(Clos(e,env)) []

let e = Abs("x",V "y")
let a = krivine(Clos(e,env)) []

let e = Abs("x",Abs("y",Plus(V "x", V "y")))
let a = krivine(Clos(e,env)) []

let e = Abs("x",Abs("y",Plus(V "x", V "z")))
let a = krivine(Clos(e,env)) []

(* Lazy evaluation *)
(* \x.((\y.3)(w)) *)
let e = Abs("x",App(Abs("y", Num 3),(Div(Num 1, Num 0))))
let a = krivine(Clos(e,env)) []

let e = Abs("x",App(Abs("y", Num 3),(Plus(Num 3, Bl true))))
let a = krivine(Clos(e,env)) []

(* Does not return ans = Vclos ("x", N 5 ) since evaluation is not eager*) 
(* In fact, this happens in case of any e in Abs("x",e) as long this exp is not applied to other *)
let e = Abs("x",Plus(Num 2, Num 3))
let a = krivine(Clos(e,env)) []

(* Ignores invalid tuple element *)
let e1 = Tuple([Div(Num 1, Num 0);Plus(Num 1, Num 0)])
let e2 = Proj(1,e1)
let a = exec e2

(* Non terminating lambda expression *)
let e1 = App(Abs("x",App(V "x",V "x")),Abs("x",App(V "x",V "x")))
let e2 = Abs("z",App(Abs("y", Num 3),e1))
let a = exec e2

(* Let expressions *)

let env = [("y",Clos(Num 2,[]));("x",Clos(Num 1,[]))]

let d1 = Adef("x",Plus(Num 2, Num 3 ))

let d2 = Adef("y",Plus(Num 2, V "x" ))

let d3 = Adef("z",Plus(Num 4, V "y" ))

let d4 = Adef("z",Plus(Num 2, V "x" ))

let d7 = Adef("x",V "y")

let d8 = Adef("y",V "x")

let d9 = Par(d7,d8)

(* Simple sequential defn *)
let exec e = krivine (Clos(e,env)) []

let el1 = elab (Seq(d1,d2)) env

let l = Let(Seq(d1,d2),Plus(V "y",Num 1))
let a = exec l
(* Simple parallel defn *)

let l = Let(Par(d1,d2),Plus(V "y",Num 1))
let a = exec l
(* Parallel defn with seemingly conflicting names *)

let l = Let(d9,Plus(V "x", V "y"))
let a = exec l

(* Sequential inside parallel *)

let se = Seq(d1,d4)
let l = Let(Par(se,d2),Plus(V "y",V "z"))
let a = exec l

(* Parallel inside Sequential *)

let par = Par(d2,d3)
let l = Let(Seq(d1,par),Times(V "y",V "z"))
let a = exec l

(* Local vs Sequential *)

let d5 = Adef("x",Num 3)

let d6 = Adef("y",Plus(Num 2, V "x" ))

let l = Let(Loc(d5,d6),Plus(V "x",V "y"))
let a = exec l

let l = Let(Seq(d5,d6),Plus(V "x",V "y"))
let a = exec l

(* let t1 = krivine (Clos((V "z"),[("z", krivine (Clos(Num(3),[])))])) [] *)
let t2 = krivine(Clos(Plus(Plus(Num(2),Num(3)),Plus(Num(2),Num(3))), [])) []
let t3 = krivine(Clos(App(Abs("x",Plus(V"x",Num(1))),Num(2)),[]))[]
let t4 = krivine(Clos(App(Abs("x", Times(V"x",Plus(V"x",Num(1)))),Num(2)),[]))[]
let t5 = krivine(Clos(App(Abs("x", App(Abs("d",Times(V"d",Num(2))),Num(2))),Num(2)),[]))[] 
let e6 = IfTE(Gt(Num(2),Num(8)),App(Abs("x", Div(V"x",Num(2))),Num(2)),App(Abs("x", Times(V"x",Plus(V"x",Num(1)))),Num(2)))
let t6 = krivine(Clos(e6,[]))[]
let e7 = IfTE(Gt(Num(2),Num(8)),Plus(Num(1), Num(2)),Div(Num(9), Num(0))) 
let t7 = krivine(Clos(e7,[]))[]














