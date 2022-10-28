type lexeme =
  |Par_ouv
  |Par_fer
  |Int of int
  |Add
  |Sub
  |Mul
  |Div
  |Mod
  |Float of float
  |Addf
  |Subf
  |Mulf
  |Int_of
  |Float_of

(* On oublie les types *)
type num =
  |Int of int
  |Float of float

type exp =
  |Val of num
  |Add of bool*exp*exp
  |Sub of bool*exp*exp
  |Conv of bool*exp
  |Mul of bool*exp*exp
  |Div of exp*exp
  |Mod of exp*exp

let rec print_exp exp =
  match exp with
  |Add (_,exp1,exp2) -> print_exp exp1; print_string " + ";  print_exp exp2
  |Sub (_,exp1,exp2) -> print_string "("; print_exp exp1; print_string ") - (";  print_exp exp2; print_string ")"
  |Val (Int n) -> print_int n
  |Val (Float x) -> print_float x
  |_ -> failwith "non traite"
  
type lex_prio0 =
  |Val of num
  |Par_ouv
  |Par_fer
  |Add0 of bool
  |Sub0 of bool
  |Mul0 of bool
  |Div0
  |Mod0
  |Conv0 of bool

type lex_prio1 =
  |Val1 of num
  |Add1 of bool
  |Sub1 of bool
  |Mul1 of bool
  |Div1
  |Mod1
  |Conv1 of bool
  |Exp1 of exp
  
type lex_prio2 =
  |Val2 of num
  |Add2 of bool
  |Sub2 of bool
  |Mul2 of bool
  |Div2
  |Exp2 of exp

type lex_prio3 =
  |Val3 of num
  |Add3 of bool
  |Sub3 of bool
  |Exp3 of exp

let conv1 x =
  match x with
  |Exp1 x -> exp
  |Val1 n -> Val n
  |_ -> failwith "unused"
  
let conv2 x =
  match x with
  |Exp2 exp -> exp
  |Val2 n -> Val n
  |_ -> failwith "unused"

let conv3 x =
  match x with
  |Exp3 exp -> exp
  |Val3 n -> Val n
  |_ -> failwith "unused"  

let rec prio2_to_prio3 lexeme2_l b =
  match lexeme2_l with
  |[Exp2 exp] -> exp
  |[Val2 n] -> Val n
  |x :: Mul3 op :: y :: ys when b -> (Exp3 (Mul (op,conv2 x,conv2 y))) :: (prio2_to_prio3 xs b)
  |x :: Mul3 op :: y :: ys when b -> (Exp3 (Mul (op,conv2 x,conv2 y))) :: (prio2_to_prio3 xs b)
and
prio3_to_exp lexeme3_l b =
  match lexeme3_l with
  |[Exp3 exp] -> exp
  |[Val3 n] -> Val n 
  |Add3 op :: xs -> prio3_to_exp xs b
  |Sub3 op :: xs -> prio3_to_exp xs (not b)
  |x :: Add3 op :: xs when b -> Add (op,conv3 x,prio3_to_exp xs b)
  |x :: Sub3 op :: xs when b -> Sub (op,conv3 x,prio3_to_exp xs (not b))
  |x :: Add3 op :: xs when not b -> Sub (op,conv3 x,prio3_to_exp xs (not b))
  |x :: Sub3 op :: xs when not b -> Add (op,conv3 x,prio3_to_exp xs b)
  |_ -> failwith "invalid expression"

let test1 = [Val2 (Int 5);Add2 true;Val2 (Int 7)];;
print_exp (prio3_to_exp (prio2_to_prio3 test1 true) true);
print_string "\n"
let test2 = [Val3 (Int 12);Sub3 true;Val3 (Int 3);Add3 true;Val3 (Int 7)];;
print_exp (prio3_to_exp test2 true);