(* On génère l'arbre syntaxique en 3 étapes:
    -on élimine les parenthèses: lexeme -> lexeme_sp
    -on élimine les opérations prioritaires: lexeme_sp -> lexeme_low_prio 
    -on élimine les opérations restantes: lexeme_low_prio -> exp *)

type lexeme =
  |Par_ouv
  |Par_fer
  |Int of int
  |Add
  |Sub
  |Mul
  |Div

type expi = Int of int | Add of expi*expi | Sub of expi*expi | Mul of expi*expi | Div of expi1*expi1 | Ofexpf of expf
and  expf = Float of float | Addf of expf*expf | Ofexpi of expi
type exp = Int_exp of expi | Float_exp of expf

type lexeme_sp =
  |Int_sp of int
  |Add_sp
  |Sub_sp
  |Mul_sp
  |Div_sp
  |Exp_sp of expi

type lexeme_low_prio =
  |Int_lp of int
  |Add_lp
  |Sub_lp
  |Exp_lp of expi

let rec lex_to_exp lexeme_l =
  lex_lp_to_exp (lex_sp_to_lex_lp (lex_to_lex_sp lexeme_l))
and
lex_to_lex_sp lexeme_l =
  let rec aux l prof acc =
    if prof = 0 then
      match l with
      |[] -> []
      |Par_ouv ::xs -> aux xs 1 []      
      |[Int n] -> [Int_sp n]
      |Par_fer :: xs -> failwith "invalid expression type 1.1"
      |Int n :: xs -> Int_sp n :: aux xs 0 []
      |Add :: xs -> Add_sp :: aux xs 0 []
      |Mul :: xs -> Mul_sp :: aux xs 0 []
    else
      match l with
      |Par_fer :: xs when prof = 1 ->  let head = lex_to_exp (List.rev acc) in
                                       Exp_sp head :: aux xs 0 []
      |Par_fer :: xs -> aux xs (prof-1) (Par_fer :: acc)
      |Par_ouv :: xs -> aux xs (prof+1) (Par_ouv :: acc)
      |x::xs -> aux xs prof (x::acc)
      |_ -> failwith "invalid expression type 1.2" in
  aux lexeme_l 0 []    
and
lex_sp_to_lex_lp lexeme_sp_l =     (*attention pb à gérer qd rajout de soustraction et division*)
  match lexeme_sp_l with
  |[Int_sp n] -> [Int_lp n] 
  |[Exp_sp exp] -> [Exp_lp exp]
  |Int_sp n :: Mul_sp :: xs -> [Exp_lp (Mul (Int n,lex_lp_to_exp (lex_sp_to_lex_lp xs)))]
  |Int_sp n :: Add_sp :: xs ->  (Int_lp n :: Add_lp :: lex_sp_to_lex_lp xs)
  |Exp_sp exp :: Mul_sp :: xs -> [Exp_lp (Mul (exp,lex_lp_to_exp (lex_sp_to_lex_lp xs)))]  
  |Exp_sp exp :: Add_sp :: xs ->  (Exp_lp exp :: Add_lp :: lex_sp_to_lex_lp xs)
  |_ -> failwith "invaliid expression type 2"
and
lex_lp_to_exp lexeme_lp_l =
  match lexeme_lp_l with
  |[Int_lp n] -> Int n
  |[Exp_lp exp] -> exp
  |Int_lp n :: Add_lp :: xs -> Add (Int n,lex_lp_to_exp xs)
  |Int_lp n :: Sub_lp :: (* code manquant *)
  |Exp_lp exp :: Add_lp :: xs -> Add (exp,lex_lp_to_exp xs)
  |_ -> failwith "invalid expression type 3";;

let rec print_expi exp =
  match exp with
  |Int n -> print_int n
  |Add (exp1,exp2) -> print_string "("; print_expi exp1; print_string ") + ("; print_expi exp2; print_string ")"
  |Mul (exp1,exp2) -> print_string "("; print_expi exp1; print_string ") * ("; print_expi exp2; print_string ")"
  |_ -> failwith "non traité actuellement";;

print_expi (lex_to_exp[Int 3; Add; Int 5; Mul; Par_ouv; Int 3; Add; Int 5; Par_fer; Mul; Int 8])
