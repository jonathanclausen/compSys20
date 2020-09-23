
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | TYPE
    | SHR
    | SAR
    | SAL
    | RPAR
    | REG of (
# 31 "parser.mly"
       (string)
# 16 "parser.ml"
  )
    | QUAD
    | PUPO of (
# 13 "parser.mly"
       (Ast.opcode)
# 22 "parser.ml"
  )
    | OBJECT
    | NUM of (
# 32 "parser.mly"
       (string)
# 28 "parser.ml"
  )
    | MOVE of (
# 12 "parser.mly"
       (Ast.opcode)
# 33 "parser.ml"
  )
    | LPAR
    | LINE
    | INC
    | IGN of (
# 29 "parser.mly"
       (string)
# 41 "parser.ml"
  )
    | ID of (
# 30 "parser.mly"
       (string)
# 46 "parser.ml"
  )
    | FUN_START
    | FUNCTION
    | EOF
    | ENDFUNCTION
    | DOLLAR
    | DIR of (
# 28 "parser.mly"
       (string)
# 56 "parser.ml"
  )
    | DEC
    | CTL3 of (
# 17 "parser.mly"
       (Ast.opcode)
# 62 "parser.ml"
  )
    | CTL2 of (
# 16 "parser.mly"
       (Ast.opcode)
# 67 "parser.ml"
  )
    | CTL1 of (
# 15 "parser.mly"
       (Ast.opcode)
# 72 "parser.ml"
  )
    | CTL0 of (
# 14 "parser.mly"
       (Ast.opcode)
# 77 "parser.ml"
  )
    | COMMA
    | COMM
    | COLON
    | ALU2 of (
# 11 "parser.mly"
       (Ast.opcode)
# 85 "parser.ml"
  )
    | ALIGN
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState117
  | MenhirState115
  | MenhirState87
  | MenhirState78
  | MenhirState75
  | MenhirState73
  | MenhirState71
  | MenhirState67
  | MenhirState65
  | MenhirState63
  | MenhirState61
  | MenhirState59
  | MenhirState6

# 1 "parser.mly"
  

# 121 "parser.ml"

let rec _menhir_goto_arg : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.op_spec) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOLLAR ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | ID _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | LPAR ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | NUM _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | REG _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
        | EOF | IGN _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, (v1 : (Ast.op_spec))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.line) = 
# 46 "parser.mly"
                                     ( Ast.Alu2(Ast.SHR, Imm("1"), v1) )
# 158 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, (v1 : (Ast.op_spec))), _, (v2 : (Ast.op_spec))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.line) = 
# 47 "parser.mly"
                                     ( Ast.Alu2(Ast.SHR, v1, v2) )
# 176 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _v
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOLLAR ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | ID _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | LPAR ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | NUM _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | REG _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
        | EOF | IGN _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, (v1 : (Ast.op_spec))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.line) = 
# 44 "parser.mly"
                                     ( Ast.Alu2(Ast.SAR, Imm("1"), v1) )
# 210 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, (v1 : (Ast.op_spec))), _, (v2 : (Ast.op_spec))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.line) = 
# 45 "parser.mly"
                                     ( Ast.Alu2(Ast.SAR, v1, v2) )
# 228 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _v
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOLLAR ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | ID _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | LPAR ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NUM _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | REG _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
        | EOF | IGN _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, (v1 : (Ast.op_spec))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.line) = 
# 48 "parser.mly"
                                     ( Ast.Alu2(Ast.SAL, Imm("1"), v1) )
# 262 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, (v1 : (Ast.op_spec))), _, (v2 : (Ast.op_spec))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.line) = 
# 49 "parser.mly"
                                     ( Ast.Alu2(Ast.SAL, v1, v2) )
# 280 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _v
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, (i : (
# 13 "parser.mly"
       (Ast.opcode)
# 289 "parser.ml"
        ))), _, (v1 : (Ast.op_spec))) = _menhir_stack in
        let _v : (Ast.line) = 
# 54 "parser.mly"
                                     ( Ast.PuPo(i, v1) )
# 294 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _v
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOLLAR ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | ID _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | LPAR ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | NUM _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | REG _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, (i : (
# 12 "parser.mly"
       (Ast.opcode)
# 333 "parser.ml"
        ))), _, (v1 : (Ast.op_spec))), _, (v2 : (Ast.op_spec))) = _menhir_stack in
        let _3 = () in
        let _v : (Ast.line) = 
# 53 "parser.mly"
                                     ( Ast.Move2(i, v1, v2) )
# 339 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _v
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, (v1 : (Ast.op_spec))) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.line) = 
# 50 "parser.mly"
                                     ( Ast.Alu2(Ast.ADD, Imm("1"), v1) )
# 350 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _v
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, (v1 : (Ast.op_spec))) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.line) = 
# 51 "parser.mly"
                                     ( Ast.Alu2(Ast.SUB, Imm("1"), v1) )
# 361 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _v
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOLLAR ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | ID _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
            | LPAR ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | NUM _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
            | REG _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, (i : (
# 11 "parser.mly"
       (Ast.opcode)
# 400 "parser.ml"
        ))), _, (v1 : (Ast.op_spec))), _, (v2 : (Ast.op_spec))) = _menhir_stack in
        let _3 = () in
        let _v : (Ast.line) = 
# 52 "parser.mly"
                                     ( Ast.Alu2(i, v1, v2) )
# 406 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _v

and _menhir_goto_aline : _menhir_env -> 'ttv_tail -> (
# 36 "parser.mly"
       (Ast.line)
# 413 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 36 "parser.mly"
       (Ast.line)
# 421 "parser.ml"
    )) = _v in
    Obj.magic _1

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 31 "parser.mly"
       (string)
# 476 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (
# 31 "parser.mly"
       (string)
# 484 "parser.ml"
    )) = _v in
    let _v : (Ast.op_spec) = 
# 94 "parser.mly"
            ( Ast.Reg(s) )
# 489 "parser.ml"
     in
    _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 32 "parser.mly"
       (string)
# 496 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | REG _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | COMMA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | NUM _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = (_menhir_stack, _v) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | RPAR ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s, (s : (
# 32 "parser.mly"
       (string)
# 537 "parser.ml"
                            ))), (s2 : (
# 31 "parser.mly"
       (string)
# 541 "parser.ml"
                            ))), (i : (
# 32 "parser.mly"
       (string)
# 545 "parser.ml"
                            ))) = _menhir_stack in
                            let _7 = () in
                            let _5 = () in
                            let _3 = () in
                            let _2 = () in
                            let _v : (Ast.op_spec) = 
# 89 "parser.mly"
                                                  ( Ast.EaDZ(s,s2,i) )
# 554 "parser.ml"
                             in
                            _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | RPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, (s : (
# 32 "parser.mly"
       (string)
# 576 "parser.ml"
                    ))), (s2 : (
# 31 "parser.mly"
       (string)
# 580 "parser.ml"
                    ))) = _menhir_stack in
                    let _5 = () in
                    let _3 = () in
                    let _2 = () in
                    let _v : (Ast.op_spec) = 
# 88 "parser.mly"
                                    ( Ast.EaDZ(s,s2,"1") )
# 588 "parser.ml"
                     in
                    _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | REG _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | REG _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | COMMA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | NUM _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_stack = (_menhir_stack, _v) in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | RPAR ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let ((((_menhir_stack, _menhir_s, (s : (
# 32 "parser.mly"
       (string)
# 638 "parser.ml"
                                ))), (s1 : (
# 31 "parser.mly"
       (string)
# 642 "parser.ml"
                                ))), (s2 : (
# 31 "parser.mly"
       (string)
# 646 "parser.ml"
                                ))), (i : (
# 32 "parser.mly"
       (string)
# 650 "parser.ml"
                                ))) = _menhir_stack in
                                let _8 = () in
                                let _6 = () in
                                let _4 = () in
                                let _2 = () in
                                let _v : (Ast.op_spec) = 
# 85 "parser.mly"
                                                           ( Ast.EaDZS(s,s1,s2,i) )
# 659 "parser.ml"
                                 in
                                _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | RPAR ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, (s : (
# 32 "parser.mly"
       (string)
# 681 "parser.ml"
                        ))), (s1 : (
# 31 "parser.mly"
       (string)
# 685 "parser.ml"
                        ))), (s2 : (
# 31 "parser.mly"
       (string)
# 689 "parser.ml"
                        ))) = _menhir_stack in
                        let _6 = () in
                        let _4 = () in
                        let _2 = () in
                        let _v : (Ast.op_spec) = 
# 84 "parser.mly"
                                             ( Ast.EaDZS(s,s1,s2,"1") )
# 697 "parser.ml"
                         in
                        _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (s : (
# 32 "parser.mly"
       (string)
# 719 "parser.ml"
                ))), (s1 : (
# 31 "parser.mly"
       (string)
# 723 "parser.ml"
                ))) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _v : (Ast.op_spec) = 
# 81 "parser.mly"
                              ( if s1 = "%rip" then Ast.EaD(s) else Ast.EaDS(s, s1) )
# 730 "parser.ml"
                 in
                _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | COMMA | EOF | IGN _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (i : (
# 32 "parser.mly"
       (string)
# 750 "parser.ml"
        ))) = _menhir_stack in
        let _v : (Ast.op_spec) = 
# 93 "parser.mly"
            ( Ast.EaD(i) )
# 755 "parser.ml"
         in
        _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REG _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NUM _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | RPAR ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s), (s2 : (
# 31 "parser.mly"
       (string)
# 800 "parser.ml"
                        ))), (i : (
# 32 "parser.mly"
       (string)
# 804 "parser.ml"
                        ))) = _menhir_stack in
                        let _6 = () in
                        let _4 = () in
                        let _2 = () in
                        let _1 = () in
                        let _v : (Ast.op_spec) = 
# 79 "parser.mly"
                                          ( Ast.EaZ(s2,i) )
# 813 "parser.ml"
                         in
                        _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), (s2 : (
# 31 "parser.mly"
       (string)
# 835 "parser.ml"
                ))) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Ast.op_spec) = 
# 77 "parser.mly"
                            ( Ast.EaZ(s2,"1") )
# 843 "parser.ml"
                 in
                _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | REG _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | REG _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | COMMA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | NUM _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = (_menhir_stack, _v) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | RPAR ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let ((((_menhir_stack, _menhir_s), (s1 : (
# 31 "parser.mly"
       (string)
# 893 "parser.ml"
                            ))), (s2 : (
# 31 "parser.mly"
       (string)
# 897 "parser.ml"
                            ))), (i : (
# 32 "parser.mly"
       (string)
# 901 "parser.ml"
                            ))) = _menhir_stack in
                            let _7 = () in
                            let _5 = () in
                            let _3 = () in
                            let _1 = () in
                            let _v : (Ast.op_spec) = 
# 78 "parser.mly"
                                                   ( Ast.EaZS(s1,s2,i) )
# 910 "parser.ml"
                             in
                            _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let ((((_menhir_stack, _menhir_s), _), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | RPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), (s1 : (
# 31 "parser.mly"
       (string)
# 932 "parser.ml"
                    ))), (s2 : (
# 31 "parser.mly"
       (string)
# 936 "parser.ml"
                    ))) = _menhir_stack in
                    let _5 = () in
                    let _3 = () in
                    let _1 = () in
                    let _v : (Ast.op_spec) = 
# 76 "parser.mly"
                                     ( Ast.EaZS(s1,s2,"1") )
# 944 "parser.ml"
                     in
                    _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (s1 : (
# 31 "parser.mly"
       (string)
# 966 "parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.op_spec) = 
# 75 "parser.mly"
                      ( Ast.EaS(s1) )
# 973 "parser.ml"
             in
            _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 30 "parser.mly"
       (string)
# 992 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | REG _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | COMMA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | NUM _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = (_menhir_stack, _v) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | RPAR ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s, (s : (
# 30 "parser.mly"
       (string)
# 1033 "parser.ml"
                            ))), (s2 : (
# 31 "parser.mly"
       (string)
# 1037 "parser.ml"
                            ))), (i : (
# 32 "parser.mly"
       (string)
# 1041 "parser.ml"
                            ))) = _menhir_stack in
                            let _7 = () in
                            let _5 = () in
                            let _3 = () in
                            let _2 = () in
                            let _v : (Ast.op_spec) = 
# 87 "parser.mly"
                                                 ( Ast.EaDZ(s,s2,i) )
# 1050 "parser.ml"
                             in
                            _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | RPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, (s : (
# 30 "parser.mly"
       (string)
# 1072 "parser.ml"
                    ))), (s2 : (
# 31 "parser.mly"
       (string)
# 1076 "parser.ml"
                    ))) = _menhir_stack in
                    let _5 = () in
                    let _3 = () in
                    let _2 = () in
                    let _v : (Ast.op_spec) = 
# 86 "parser.mly"
                                   ( Ast.EaDZ(s,s2,"1") )
# 1084 "parser.ml"
                     in
                    _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | REG _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | REG _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | COMMA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | NUM _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_stack = (_menhir_stack, _v) in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | RPAR ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let ((((_menhir_stack, _menhir_s, (s : (
# 30 "parser.mly"
       (string)
# 1134 "parser.ml"
                                ))), (s1 : (
# 31 "parser.mly"
       (string)
# 1138 "parser.ml"
                                ))), (s2 : (
# 31 "parser.mly"
       (string)
# 1142 "parser.ml"
                                ))), (i : (
# 32 "parser.mly"
       (string)
# 1146 "parser.ml"
                                ))) = _menhir_stack in
                                let _8 = () in
                                let _6 = () in
                                let _4 = () in
                                let _2 = () in
                                let _v : (Ast.op_spec) = 
# 83 "parser.mly"
                                                          ( Ast.EaDZS(s,s1,s2,i) )
# 1155 "parser.ml"
                                 in
                                _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | RPAR ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, (s : (
# 30 "parser.mly"
       (string)
# 1177 "parser.ml"
                        ))), (s1 : (
# 31 "parser.mly"
       (string)
# 1181 "parser.ml"
                        ))), (s2 : (
# 31 "parser.mly"
       (string)
# 1185 "parser.ml"
                        ))) = _menhir_stack in
                        let _6 = () in
                        let _4 = () in
                        let _2 = () in
                        let _v : (Ast.op_spec) = 
# 82 "parser.mly"
                                            ( Ast.EaDZS(s,s1,s2,"1") )
# 1193 "parser.ml"
                         in
                        _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (s : (
# 30 "parser.mly"
       (string)
# 1215 "parser.ml"
                ))), (s1 : (
# 31 "parser.mly"
       (string)
# 1219 "parser.ml"
                ))) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _v : (Ast.op_spec) = 
# 80 "parser.mly"
                             ( if s1 = "%rip" then Ast.EaD(s) else Ast.EaDS(s, s1) )
# 1226 "parser.ml"
                 in
                _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | COMMA | EOF | IGN _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (s : (
# 30 "parser.mly"
       (string)
# 1246 "parser.ml"
        ))) = _menhir_stack in
        let _v : (Ast.op_spec) = 
# 92 "parser.mly"
            ( Ast.EaD(s) )
# 1251 "parser.ml"
         in
        _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (s : (
# 30 "parser.mly"
       (string)
# 1274 "parser.ml"
        )) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.op_spec) = 
# 90 "parser.mly"
                   ( Ast.Imm(s) )
# 1281 "parser.ml"
         in
        _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
    | NUM _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (i : (
# 32 "parser.mly"
       (string)
# 1291 "parser.ml"
        )) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.op_spec) = 
# 91 "parser.mly"
                   ( Ast.Imm(i) )
# 1298 "parser.ml"
         in
        _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> (Ast.line) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, (i : (Ast.line))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 36 "parser.mly"
       (Ast.line)
# 1323 "parser.ml"
        ) = 
# 39 "parser.mly"
                       ( i )
# 1327 "parser.ml"
         in
        _menhir_goto_aline _menhir_env _menhir_stack _v
    | IGN _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, (i : (Ast.line))), (_2 : (
# 29 "parser.mly"
       (string)
# 1342 "parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _v : (
# 36 "parser.mly"
       (Ast.line)
# 1348 "parser.ml"
            ) = 
# 40 "parser.mly"
                           ( i )
# 1352 "parser.ml"
             in
            _menhir_goto_aline _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and aline : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 36 "parser.mly"
       (Ast.line)
# 1381 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ALIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | NUM _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (i : (
# 32 "parser.mly"
       (string)
# 1407 "parser.ml"
            )) = _v in
            let _1 = () in
            let _v : (Ast.line) = 
# 63 "parser.mly"
                                     ( Ast.Align(i) )
# 1413 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | ALU2 _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOLLAR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | ID _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
        | LPAR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | NUM _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
        | REG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
    | COMM ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | NUM _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | COMMA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | NUM _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (aln : (
# 32 "parser.mly"
       (string)
# 1475 "parser.ml"
                            )) = _v in
                            let ((_menhir_stack, (s : (
# 30 "parser.mly"
       (string)
# 1480 "parser.ml"
                            ))), (sz : (
# 32 "parser.mly"
       (string)
# 1484 "parser.ml"
                            ))) = _menhir_stack in
                            let _5 = () in
                            let _3 = () in
                            let _1 = () in
                            let _v : (Ast.line) = 
# 62 "parser.mly"
                                               ( Ast.Comm(s, int_of_string sz, int_of_string aln) )
# 1492 "parser.ml"
                             in
                            _menhir_goto_instruction _menhir_env _menhir_stack _v
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            raise _eRR)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | CTL0 _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (i : (
# 14 "parser.mly"
       (Ast.opcode)
# 1527 "parser.ml"
        )) = _v in
        let _v : (Ast.line) = 
# 60 "parser.mly"
                                     ( Ast.Ctl0(i) )
# 1532 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _v
    | CTL1 _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (v1 : (
# 30 "parser.mly"
       (string)
# 1548 "parser.ml"
            )) = _v in
            let (_menhir_stack, (i : (
# 15 "parser.mly"
       (Ast.opcode)
# 1553 "parser.ml"
            ))) = _menhir_stack in
            let _v : (Ast.line) = 
# 57 "parser.mly"
                                     ( Ast.Ctl1(i, EaD(v1)) )
# 1558 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _v
        | REG _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (ri : (
# 31 "parser.mly"
       (string)
# 1568 "parser.ml"
            )) = _v in
            let (_menhir_stack, (i : (
# 15 "parser.mly"
       (Ast.opcode)
# 1573 "parser.ml"
            ))) = _menhir_stack in
            let _v : (Ast.line) = 
# 58 "parser.mly"
                                     ( Ast.Ctl1(i, Reg(ri)) )
# 1578 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | CTL2 _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | REG _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (ri : (
# 31 "parser.mly"
       (string)
# 1610 "parser.ml"
                    )) = _v in
                    let ((_menhir_stack, (i : (
# 16 "parser.mly"
       (Ast.opcode)
# 1615 "parser.ml"
                    ))), (im : (
# 30 "parser.mly"
       (string)
# 1619 "parser.ml"
                    ))) = _menhir_stack in
                    let _3 = () in
                    let _v : (Ast.line) = 
# 59 "parser.mly"
                                     ( Ast.Ctl2(i, EaD(im), Reg(ri)) )
# 1625 "parser.ml"
                     in
                    _menhir_goto_instruction _menhir_env _menhir_stack _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | CTL3 _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOLLAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | NUM _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | COMMA ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | REG _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = (_menhir_stack, _v) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | COMMA ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | ID _v ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let (t : (
# 30 "parser.mly"
       (string)
# 1683 "parser.ml"
                                )) = _v in
                                let (((_menhir_stack, (i : (
# 17 "parser.mly"
       (Ast.opcode)
# 1688 "parser.ml"
                                ))), (v1 : (
# 32 "parser.mly"
       (string)
# 1692 "parser.ml"
                                ))), (v2 : (
# 31 "parser.mly"
       (string)
# 1696 "parser.ml"
                                ))) = _menhir_stack in
                                let _6 = () in
                                let _4 = () in
                                let _2 = () in
                                let _v : (Ast.line) = 
# 55 "parser.mly"
                                                         ( Ast.Ctl3(i, Ast.Imm(v1), Ast.Reg(v2), Ast.EaD(t)) )
# 1704 "parser.ml"
                                 in
                                _menhir_goto_instruction _menhir_env _menhir_stack _v
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                raise _eRR)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            raise _eRR)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | REG _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | REG _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | COMMA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | ID _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (t : (
# 30 "parser.mly"
       (string)
# 1761 "parser.ml"
                            )) = _v in
                            let (((_menhir_stack, (i : (
# 17 "parser.mly"
       (Ast.opcode)
# 1766 "parser.ml"
                            ))), (v1 : (
# 31 "parser.mly"
       (string)
# 1770 "parser.ml"
                            ))), (v2 : (
# 31 "parser.mly"
       (string)
# 1774 "parser.ml"
                            ))) = _menhir_stack in
                            let _5 = () in
                            let _3 = () in
                            let _v : (Ast.line) = 
# 56 "parser.mly"
                                                  ( Ast.Ctl3(i, Ast.Reg(v1), Ast.Reg(v2), Ast.EaD(t)) )
# 1781 "parser.ml"
                             in
                            _menhir_goto_instruction _menhir_env _menhir_stack _v
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            raise _eRR)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | DEC ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOLLAR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | ID _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | LPAR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | NUM _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | REG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
    | DIR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (s : (
# 28 "parser.mly"
       (string)
# 1835 "parser.ml"
        )) = _v in
        let _v : (Ast.line) = 
# 64 "parser.mly"
                                     ( Ast.Directive(s) )
# 1840 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _v
    | ENDFUNCTION ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (Ast.line) = 
# 68 "parser.mly"
                                     ( Ast.Fun_end )
# 1851 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _v
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (Ast.line) = 
# 71 "parser.mly"
                                     ( Ast.Ignored("") )
# 1862 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _v
    | FUN_START ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (Ast.line) = 
# 69 "parser.mly"
                                     ( Ast.Fun_start )
# 1873 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _v
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, (k : (
# 30 "parser.mly"
       (string)
# 1889 "parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.line) = 
# 43 "parser.mly"
                                     ( Ast.Label(k) )
# 1895 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | IGN _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (s : (
# 29 "parser.mly"
       (string)
# 1910 "parser.ml"
        )) = _v in
        let _v : (Ast.line) = 
# 65 "parser.mly"
                                     ( Ast.Ignored(s) )
# 1915 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _v
    | INC ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOLLAR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | ID _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | LPAR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | NUM _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | REG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | LINE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (Ast.line) = 
# 70 "parser.mly"
                                     ( Ast.Ignored("") )
# 1945 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _v
    | MOVE _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOLLAR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | ID _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | LPAR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | NUM _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | REG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | PUPO _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOLLAR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | ID _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | LPAR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | NUM _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | REG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | QUAD ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | NUM _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (i : (
# 32 "parser.mly"
       (string)
# 2000 "parser.ml"
            )) = _v in
            let _1 = () in
            let _v : (Ast.line) = 
# 61 "parser.mly"
                                     ( Ast.Quad(i) )
# 2006 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | SAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOLLAR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | ID _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | LPAR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | NUM _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | REG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | SAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOLLAR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | ID _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | LPAR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | NUM _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | REG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | SHR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOLLAR ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | ID _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | LPAR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | NUM _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | REG _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6)
    | TYPE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FUNCTION ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, (s : (
# 30 "parser.mly"
       (string)
# 2094 "parser.ml"
                    ))) = _menhir_stack in
                    let _4 = () in
                    let _3 = () in
                    let _1 = () in
                    let _v : (Ast.line) = 
# 66 "parser.mly"
                                     ( Ast.Function(s) )
# 2102 "parser.ml"
                     in
                    _menhir_goto_instruction _menhir_env _menhir_stack _v
                | OBJECT ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, (s : (
# 30 "parser.mly"
       (string)
# 2112 "parser.ml"
                    ))) = _menhir_stack in
                    let _4 = () in
                    let _3 = () in
                    let _1 = () in
                    let _v : (Ast.line) = 
# 67 "parser.mly"
                                     ( Ast.Object(s) )
# 2120 "parser.ml"
                     in
                    _menhir_goto_instruction _menhir_env _menhir_stack _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR)

# 233 "/home/neumann/.opam/4.07.0/lib/menhir/standard.mly"
  

# 2147 "parser.ml"
