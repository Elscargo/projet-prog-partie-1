open Lexeur

(* On oublie les types *)
type num =
  |Int of int
  |Float of float

type exp =
  |Vale of num
  |Adde of bool*exp*exp
  |Sube of bool*exp*exp
  |Conve of bool*exp
  |Mule of bool*exp*exp
  |Dive of exp*exp
  |Mode of exp*exp
 
let rec print_exp exp =
  match exp with
  |Adde (_,exp1,exp2) -> print_string "("; print_exp exp1; print_string ") + (";  print_exp exp2; print_string ")"
  |Sube (_,exp1,exp2) -> print_string "("; print_exp exp1; print_string ") - (";  print_exp exp2; print_string ")"
  |Vale (Int n) -> print_int n
  |Vale (Float x) -> print_float x
  |Mule (_,exp1,exp2) -> print_string "("; print_exp exp1; print_string ") * (";  print_exp exp2; print_string ")"
  |Dive (exp1,exp2) -> print_string "("; print_exp exp1; print_string ") / (";  print_exp exp2; print_string ")"
  |Mode (exp1,exp2) -> print_string "("; print_exp exp1; print_string ") mod (";  print_exp exp2; print_string ")"
  |Conve (_,exp) -> print_string "of ("; print_exp exp; print_string ")s"

type lex_exp = (* on créer un type intermédiare pouvant accepter les exceptions *)
  |Val of num
  |Par_ouv
  |Par_fer
  |Add of bool
  |Sub of bool
  |Mul of bool
  |Div
  |Mod
  |Conv of bool
  |Exp of exp

let lex_to_lex_exp l =
  match l with
  |Int_of -> Conv true
  |Int n -> Val (Int n)
  |Float x -> Val (Float x)
  |Par_ouv -> Par_ouv
  |Par_fer -> Par_fer
  |Add -> Add true
  |Addf -> Add false
  |Sub -> Sub true
  |Subf -> Sub false
  |Mul -> Mul true
  |Mulf -> Mul false
  |Div -> Div
  |Mod -> Mod
  |Float_of -> Conv false  

let prio lex =
  match lex with
  |Val _ -> (-1)
  |Par_ouv -> 3
  |Par_fer -> 3
  |Add _ -> 0
  |Sub _ -> 0
  |Mul _ -> 1
  |Div -> 1
  |Mod -> 1
  |Conv _ -> 2
  |Exp _ -> (-1)                 

let ntm arg =
  match arg with
  |Val x -> Vale x
  |Exp exp -> exp
  |_ -> failwith "error ntm"  
let conv op arg1 arg2 =
  match op,arg1 with
  |Add b,Some arg -> Adde (b,ntm arg,ntm arg2)
  |Sub b,Some arg -> Sube (b,ntm arg,ntm arg2)
  |Mul b,Some arg -> Mule (b,ntm arg,ntm arg2)
  |Div,Some arg -> Dive(ntm arg,ntm arg2)
  |Mod,Some arg -> Mode (ntm arg,ntm arg2)
  |Conv b,_ -> Conve (b,ntm arg2)
  |_ -> failwith "impossible1"

let rec split l prof acc = (* dans le cas ou l'on croise une parenthese on parcours la liste pour récupérer l'expression que l'on recherche *)
  match l with
  |[] -> failwith "impossible2"
  |Par_ouv :: xs -> split xs (prof+1) (Par_ouv :: acc)
  |Par_fer :: xs -> if prof = 1 then (List.rev acc),xs else split xs (prof-1) (Par_fer :: acc)
  |x::xs -> split xs prof (x :: acc)
  
let build l =
  let rec aux1 l prev p acc =
    match l with
    |Par_ouv :: xs -> let exp1,l1 = split xs 1 [] in aux1 ((aux1 exp1 None 2 []) :: l1) prev p acc
    |x :: Par_ouv :: xs -> let exp1,l1 = split xs 1 [] in aux1 (x :: (aux1 exp1 None 2 []) :: l1) prev p acc
    |x :: y :: xs when (prio x) = p -> aux1  xs (Some (Exp (conv x prev y))) p acc
    |x :: xs when (prio x) = (-1) -> aux1 xs (Some x) p acc
    |x :: xs -> begin match prev with
                |None -> aux1 xs None p (x::acc)
                |Some a -> aux1 xs None p (x::a::acc) end 
    |[] -> begin match prev,p with
                |None,0 ->  List.hd acc
                |Some x,0 -> x
                |None,_ -> aux1 (List.rev acc) None (p-1) []
                |Some x,_ -> aux1 (List.rev (x::acc)) None (p-1) [] end in
  match (aux1 l None 2 []) with
  |Exp exp -> exp
  |Val n -> Vale n
  |_ -> failwith "impossible3"