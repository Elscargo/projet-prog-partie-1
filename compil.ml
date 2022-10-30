open Format
open X86_64
open Type

let evali exp t =
  let rec aux exp =
    match exp with
    |Int n -> movq (imm n) (reg rdi)
    |Add (exp1,exp2) -> (aux exp1) ++ pushq (reg rdi) ++ (aux exp2) ++ popq rsi ++ addq (reg rsi) (reg rdi)
    |Mul (exp1,exp2) -> (aux exp1) ++ pushq (reg rdi) ++ (aux exp2) ++ popq rsi ++  imulq (reg rsi) (reg rdi)
    |Sub (exp1,exp2) -> (aux exp1) ++ pushq (reg rdi) ++ (aux exp2) ++ popq rsi ++ subq (reg rdi) (reg rsi) ++ movq (reg rsi) (reg rdi)
    |Mod (exp1,exp2) -> (aux exp1) ++ pushq (reg rdi) ++ (aux exp2) ++ popq rsi ++ movq (imm 0) (reg rdx) ++ movq (reg rsi) (reg rax) ++ idivq (reg rdi) ++ movq (reg rdx) (reg rdi)
    |Div (exp1,exp2) -> (aux exp1) ++ pushq (reg rdi) ++ (aux exp2) ++ popq rsi ++ movq (imm 0) (reg rdx) ++ movq (reg rsi) (reg rax) ++ idivq (reg rdi) ++ movq (reg rax) (reg rdi)    
    |_ -> failwith "non traité" in
  let code = {text = globl "main" ++ label "main" ++ (aux exp) ++ call "print_int" ++ ret ++
  inline "
          print_int:
          movq %rdi, %rsi
          movq $S_int, %rdi
          xorq %rax, %rax
          call printf
          movq %rsi, %rdi
          sum:
          addq %rsi, %rdi
          ret
          ";
          data = label "S_int" ++ string "%d\n";} in
  let c = open_out t in
  let fmt = formatter_of_out_channel c in
  X86_64.print_program fmt code;
  close_out c;;