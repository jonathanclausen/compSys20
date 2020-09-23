{
module L = Lexing
module B = Buffer

open Parser

let get = L.lexeme
let sprintf = Printf.sprintf

let position lexbuf =
    let p = lexbuf.L.lex_curr_p in
        sprintf "%s:%d:%d" 
        p.L.pos_fname p.L.pos_lnum (p.L.pos_cnum - p.L.pos_bol)

exception Error of string

let error lexbuf fmt = 
    Printf.kprintf (fun msg -> 
        raise (Error ((position lexbuf)^" "^msg))) fmt

let translating = ref false

let zap_dollar s = String.sub s 1 ((String.length s) - 1)

}

let ws = [' ' '\t']
let nl = ['\n']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_' '.']
let id    = alpha (alpha|digit)*
let num   = '-'? digit+
let start_proc = ".cfi_startproc"
let directive = ".text" | ".globl" | ".size" | ".section" | ".file" | ".ident"
    |  ".p2align" |  ".data" 
let ignored = ".cfi_def_cfa_offset" | ".cfi_offset" | ".cfi_remember_state" | ".cfi_restore" 
    | ".cfi_def_cfa" | ".cfi_def_cfa_register" | ".subsections_via_symbols"

let regs8 = "%al" | "%bl" | "%cl" | "%dl" | "%bpl" | "%sil" | "%dil" | "%spl"
    | "%r8l" | "%r9l" | "%r10l" | "%r11l" | "%r12l" | "%r13l" | "%r14l" | "%r15l"

let regs32 = "%eax" | "%ebx" | "%ecx" | "%edx" | "%ebp" | "%esi" | "%edi" | "%esp"
    | "%r8d" | "%r9d" | "%r10d" | "%r11d" | "%r12d" | "%r13d" | "%r14d" | "%r15d"

let regs64 = "%rax" | "%rbx" | "%rcx" | "%rdx" | "%rbp" | "%rsi" | "%rdi" | "%rsp"
    | "%r8" | "%r9" | "%r10" | "%r11" | "%r12" | "%r13" | "%r14" | "%r15"

    | "%rip"      (* <--- translated at parser level to different addressing mode *)

rule read = parse
| ws+       { read lexbuf  }
| ".align\t16, 0x90" { read lexbuf }
| ".text"   { DIR(".text") }
| ".type"   { TYPE }
| "@function" { FUNCTION }
| ".cfi_endproc" { ENDFUNCTION }
| "@object" { OBJECT }
| directive [^'\n']*  { DIR(get lexbuf) }
| ignored [^'\n']* { IGN(get lexbuf) }
| '#' [^'\n']* { read lexbuf }
| start_proc { FUN_START }
| nl        { L.new_line lexbuf; LINE  }
| '('       { LPAR          }
| ')'       { RPAR          }
| ','       { COMMA         }
| ':'       { COLON         }
| regs64    { REG(get lexbuf) }
| regs32    { REG(get lexbuf) }
| regs8     { REG(get lexbuf) }
| "movabsq" { ALU2(MOVABSQ) }
| "leaq"    { ALU2(LEA)   }
| "addq"     { ALU2(ADD)   }
| "subq"     { ALU2(SUB)   }
| "andq"     { ALU2(AND)   }
| "orq"      { ALU2(OR)    }
| "xorq"     { ALU2(XOR)   }
| "addl"     { ALU2(ADD)   }
| "subl"     { ALU2(SUB)   }
| "andl"     { ALU2(AND)   }
| "incq"     { INC }
| "decq"     { DEC }
| "sarq"     { SAR }
| "salq"     { SAL }
| "shlq"     { SAL }
| "shrq"     { SHR }
| "shrl"     { SHR } (* treat as shrq - this should be ok for code generated by gcc *)
| "orl"      { ALU2(OR)    }
| "xorl"     { ALU2(XOR)   }
| "testq"     { ALU2(TEST)   }
| "testl"     { ALU2(TEST)   }
| "cmpq"     { ALU2(CMP)   }
| "cmpl"     { ALU2(CMP)   }
| "movl"    { MOVE(MOV) }
| "movq"    { MOVE(MOV) }
| "syscall"      { CTL0(SYSCALL) }
| "rep ret" { CTL0(RET) }
| "ret"     { if !translating then CTL0(RET) else CTL1(RET) }
| "retq"     { if !translating then CTL0(RET) else CTL1(RET) }
| "jne"     { CTL1(Jcc(NE)) }
| "je"      { CTL1(Jcc(E))  }
| "jle"     { CTL1(Jcc(LE)) }
| "jl"      { CTL1(Jcc(L))  }
| "js"      { CTL1(Jcc(L))  } (* treat js as jl - this should be ok for the code generated by gcc *)
| "jns"      { CTL1(Jcc(GE))  } (* treat jns as jge - this should be ok for the code generated by gcc *)
| "jge"     { CTL1(Jcc(GE)) }
| "jg"     { CTL1(Jcc(G)) }
| "jb"      { CTL1(Jcc(B)) }
| "jbe"      { CTL1(Jcc(BE)) }
| "jna"      { CTL1(Jcc(BE)) }
| "ja"      { CTL1(Jcc(A)) }
| "jae"      { CTL1(Jcc(AE)) }
| "jnb"      { CTL1(Jcc(AE)) }
| "cmovne"     { ALU2(CMOVcc(NE)) }
| "cmove"      { ALU2(CMOVcc(E))  }
| "cmovle"     { ALU2(CMOVcc(LE)) }
| "cmovl"      { ALU2(CMOVcc(L))  }
| "cmovs"      { ALU2(CMOVcc(L))  } (* treat cmovs as cmovl - this should be ok for the code generated by gcc *)
| "cmovns"      { ALU2(CMOVcc(GE))  } (* treat cmovns as cmovge - this should be ok for the code generated by gcc *)
| "cmovge"     { ALU2(CMOVcc(GE)) }
| "cmovg"     { ALU2(CMOVcc(G)) }
| "cmovb"      { ALU2(CMOVcc(B)) }
| "cmovbe"      { ALU2(CMOVcc(BE)) }
| "cmovna"      { ALU2(CMOVcc(BE)) }
| "cmova"      { ALU2(CMOVcc(A)) }
| "cmovae"      { ALU2(CMOVcc(AE)) }
| "cmovnb"      { ALU2(CMOVcc(AE)) }
| "cbne"     { CTL3(CBcc(NE)) }
| "cbe"      { CTL3(CBcc(E))  }
| "cble"     { CTL3(CBcc(LE)) }
| "cbl"      { CTL3(CBcc(L))  }
| "cbge"     { CTL3(CBcc(GE)) }
| "cbg"     { CTL3(CBcc(G)) }
| "cba"     { CTL3(CBcc(A)) }
| "cbae"     { CTL3(CBcc(AE)) }
| "cbb"     { CTL3(CBcc(B)) }
| "cbbe"     { CTL3(CBcc(BE)) }
| "jmp"     { CTL1(JMP) }
| "call"    { if !translating then CTL1(CALL) else CTL2(CALL) }
| "callq"    { if !translating then CTL1(CALL) else CTL2(CALL) }
| "pushq"    { PUPO(PUSH) }
| "popq"     { PUPO(POP) }
| "imulq"   { ALU2(IMUL) }
| "mulq"   { ALU2(MUL) }
| ".quad"   { QUAD }
| ".comm"   { COMM }
| ".align"  { ALIGN }
| "$"       { DOLLAR }
| num       { NUM(get lexbuf) }
| id        { ID(get lexbuf)}
| eof       { EOF           }
| _         { raise (Error (Printf.sprintf "unhandled '%s' - in: " (get lexbuf))) }
