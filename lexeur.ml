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

let convert str is_float =
  try
    match str with
    |"+" -> Add
    |"-" -> Sub
    |"*" -> Mul
    |"/" -> Div
    |"%" -> Mod
    |"(" -> Par_ouv
    |")" -> Par_fer
    |"+." -> Addf
    |"-." -> Subf
    |"*." -> Mulf
    |"INT" -> Int_of
    |"FLOAT" -> Float_of
    |_ when is_float -> Float (float_of_string str)
    |_ -> Int (int_of_string str)
  with
  |Failure _ -> failwith "Not a correct expression";;

let flag_of s = (* on définit ici les types de chaque caractère, le . est spécial car il peut etre dans les opérateurs et dans les nombres *)
  match s with
  |'+' |'-' |'*' |'/' |'%'  -> 1
  |' ' |'(' |')' -> -1
  |'.' -> 3
  |'I' |'N' |'T' |'F' |'L' |'O' |'A' -> 2
  |'0' |'1' |'2' |'3' |'4' |'5' |'6' |'7' |'8' |'9' -> 0
  |_ -> failwith "Unknown character";;

let app c s = if c <> ' ' then s:= (!s)^(String.make 1 c);;

let from_text_to_lexeme txt =
  let n = String.length txt in
  let s = String.uppercase_ascii txt in
  let cur = ref "" in (* enregistre le terme que l'on traite *)
  let lexeme_l = ref [] in
  let flag = ref (-1) in (* flag=0 -> cur=int or float; flag=1 -> cur=op; flag=-1 -> cur=""; flag=2 -> cur=type *)
  let is_float = ref false in
  let add_to_lexeme () = if !cur <> "" then (lexeme_l := (convert (!cur) (!is_float))::(!lexeme_l); cur := ""; flag := -1; is_float := false) in
  for i=0 to n-1 do
      let f = flag_of s.[i] in
      if f = 3 then
        if (!flag = 0) || (!flag = -1) then
          (flag := 0;
          is_float := true;
          app s.[i] cur)
        else if !flag = 1 then
          app s.[i] cur
        else
          failwith "Wrong character ."
      else if f = !flag then
        app s.[i] cur
      else begin
        add_to_lexeme();    
        app s.[i] cur;
        flag := f  end  
  done;
  add_to_lexeme();
  List.rev (!lexeme_l);;

let print_lex lex =
  match lex with
  |Par_ouv -> print_string "Par_ouv "
  |Par_fer -> print_string "Par_fer "
  |Int n -> print_int n; print_string " " 
  |Add -> print_string "Add "
  |Sub -> print_string "Sub "
  |Mul -> print_string "Mul "
  |Div -> print_string "Div "
  |Mod -> print_string "Mod "
  |Float x -> print_float x; print_string " "
  |Addf -> print_string "Addf "
  |Subf -> print_string "Subf "
  |Mulf -> print_string "Mulf "
  |Int_of -> print_string "Int "
  |Float_of -> print_string "Float ";;

let print_t txt =
  let l = from_text_to_lexeme txt in
  List.iter print_lex l;
  print_string "\n";;