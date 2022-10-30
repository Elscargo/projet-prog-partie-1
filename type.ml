open Parseur

type exp = Int of int | Add of exp*exp | Sub of exp*exp | Div of exp*exp | Mod of exp*exp | Mul of exp*exp | Int_of of exp 
  | Float of float | Addf of exp*exp | Subf of exp*exp | Mulf of exp*exp | Float_of of exp
  
let type_of expt =
  match expt with
  |Vale (Int _) -> true
  |Vale (Float _) -> false
  |Adde (b,_,_) |Sube (b,_,_) |Conve (b,_) |Mule (b,_,_) -> b
  |_ -> true
let rec verif expt =
  match expt with
  |Vale (Int n) -> Int n 
  |Vale (Float x) -> Float x
  |Sube (b,expt1,expt2) -> if b && (type_of expt1) && (type_of expt2) then Sub (verif expt1,verif expt2) else 
          if (not b) && (not (type_of expt1)) && (not (type_of expt2)) then Subf (verif expt1,verif expt2) else failwith "nope"
  |Adde (b,expt1,expt2) -> if b && (type_of expt1) && (type_of expt2) then Add (verif expt1,verif expt2) else 
          if (not b) && (not (type_of expt1)) && (not (type_of expt2)) then Addf (verif expt1,verif expt2) else failwith "mal typé"
  |Mule (b,expt1,expt2) -> if b && (type_of expt1) && (type_of expt2) then Mul (verif expt1,verif expt2) else 
          if (not b) && (not (type_of expt1)) && (not (type_of expt2)) then Mulf (verif expt1,verif expt2) else failwith "mal typé"
  |Dive (expt1,expt2) -> if (type_of expt1) && (type_of expt2) then Div (verif expt1,verif expt2) else failwith "mal typé"
  |Conve (b,expt1) -> if b && (not (type_of expt1)) then Int_of (verif expt1) else
          if (not b) && (type_of expt1) then Float_of (verif expt1) else failwith "mal typé"
  |Mode (expt1,expt2) -> if (type_of expt1) && (type_of expt2) then Add (verif expt1,verif expt2) else failwith "mal typé"                                