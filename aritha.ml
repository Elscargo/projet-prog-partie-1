open Compil
open String


let compil_exp exp_s file_s =
  let lexems = Lexeur.from_text_to_lexeme exp_s in
  let ast = Parseur.build (List.map Parseur.lex_to_lex_exp lexems) in
  let tast = Type.verif ast in
  Compil.evali tast file_s 
let _ =
  let exp_file = Sys.argv.(1) in
  let ic = open_in exp_file in
  let line = input_line ic in
  let file_s = (String.sub exp_file 0 (String.index exp_file '.' ))^".s" in
  print_endline line;
  compil_exp line file_s;
  flush stdout;
  close_in ic

