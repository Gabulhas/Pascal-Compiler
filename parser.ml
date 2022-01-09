
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
  type token = 
    | WRITE
    | WHILE
    | VAR
    | TSTRING
    | TRUE
    | TO
    | TINT
    | TIMES
    | THEN
    | TCHAR
    | TBOOLEAN
    | SEMICOLON
    | RS
    | RP
    | READ
    | PSTRING of (
# 15 "parser.mly"
       (string)
# 29 "parser.ml"
  )
    | PROGRAM
    | PROCEDURE
    | PLUS
    | PCHAR of (
# 16 "parser.mly"
       (char)
# 37 "parser.ml"
  )
    | PBOOLEAN of (
# 14 "parser.mly"
       (bool)
# 42 "parser.ml"
  )
    | OR
    | OF
    | NOT
    | MODU
    | MINUS
    | LS
    | LP
    | LESSEQUAL
    | LESS
    | INT of (
# 13 "parser.mly"
       (int)
# 56 "parser.ml"
  )
    | IF
    | IDE of (
# 12 "parser.mly"
       (string)
# 62 "parser.ml"
  )
    | GREATEREQUAL
    | GREATER
    | FUNCTION
    | FOR
    | FALSE
    | EQUAL
    | EOF
    | END
    | ELSE
    | DOWNTO
    | DOT
    | DO
    | DIVISION
    | COMMA
    | COLON
    | BEGIN
    | ASSIGN
    | ARRAY
    | AND
  
end

include MenhirBasics

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState152
  | MenhirState149
  | MenhirState147
  | MenhirState142
  | MenhirState139
  | MenhirState133
  | MenhirState131
  | MenhirState130
  | MenhirState128
  | MenhirState126
  | MenhirState124
  | MenhirState123
  | MenhirState121
  | MenhirState118
  | MenhirState113
  | MenhirState111
  | MenhirState109
  | MenhirState100
  | MenhirState95
  | MenhirState93
  | MenhirState91
  | MenhirState89
  | MenhirState87
  | MenhirState85
  | MenhirState83
  | MenhirState81
  | MenhirState79
  | MenhirState77
  | MenhirState75
  | MenhirState73
  | MenhirState70
  | MenhirState68
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState59
  | MenhirState57
  | MenhirState54
  | MenhirState51
  | MenhirState49
  | MenhirState46
  | MenhirState44
  | MenhirState42
  | MenhirState38
  | MenhirState35
  | MenhirState32
  | MenhirState30
  | MenhirState29
  | MenhirState26
  | MenhirState20
  | MenhirState8
  | MenhirState5
  | MenhirState3
  | MenhirState2

# 1 "parser.mly"
   (* HEADER *)

open Ast;;



let vars_to_list var_ides var_type =
    List.map (fun x -> VariableDeclaration(x, var_type)) var_ides


# 162 "parser.ml"

[@@@ocaml.warning "-4-39"]

let rec _menhir_goto_subprogram_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.subprogram_declaration list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Ast.subprogram_declaration list)) = _v in
        let _v : (Ast.subprogram_declaration list) = 
# 154 "parser.mly"
                                                        ( _1 )
# 176 "parser.ml"
         in
        _menhir_goto_opt_subprogram_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Ast.subprogram_declaration list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Ast.subprogram_declaration))) = _menhir_stack in
        let _v : (Ast.subprogram_declaration list) = 
# 159 "parser.mly"
                                                     ( _1::_2 )
# 187 "parser.ml"
         in
        _menhir_goto_subprogram_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_parameter_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.variable_declaration list list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState46 | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.variable_declaration list list)) = _v in
        let _v : (Ast.variable_declaration list list) = 
# 141 "<standard.mly>"
    ( x )
# 203 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_parameter__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.variable_declaration list list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.variable_declaration list))) = _menhir_stack in
        let _v : (Ast.variable_declaration list list) = 
# 240 "<standard.mly>"
    ( x :: xs )
# 214 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_parameter_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_variable_declaration_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.variable_declaration list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Ast.variable_declaration list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Ast.variable_declaration list))) = _menhir_stack in
        let _v : (Ast.variable_declaration list) = 
# 90 "parser.mly"
                                                ( _1@_2 )
# 231 "parser.ml"
         in
        _menhir_goto_variable_declaration_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Ast.variable_declaration list)) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Ast.variable_declaration list) = 
# 85 "parser.mly"
                                                      ( _2 )
# 242 "parser.ml"
         in
        _menhir_goto_opt_variable_declaration_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_subprogram : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.subprogram_declaration) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUNCTION ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | PROCEDURE ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Ast.subprogram_declaration))) = _menhir_stack in
        let _v : (Ast.subprogram_declaration list) = 
# 158 "parser.mly"
                                                                 ( [_1] )
# 265 "parser.ml"
         in
        _menhir_goto_subprogram_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_goto_ptype : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.pascaltype) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.ident list))), _, (_3 : (Ast.pascaltype))) = _menhir_stack in
            let _v : (Ast.variable_declaration list) = 
# 94 "parser.mly"
                                                                (vars_to_list _1 _3)
# 290 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
            | BEGIN | FUNCTION | PROCEDURE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (_1 : (Ast.variable_declaration list))) = _menhir_stack in
                let _v : (Ast.variable_declaration list) = 
# 89 "parser.mly"
                                         ( _1 )
# 305 "parser.ml"
                 in
                _menhir_goto_variable_declaration_list _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.ident list))), _, (_3 : (Ast.pascaltype))) = _menhir_stack in
        let _v : (Ast.variable_declaration list) = 
# 170 "parser.mly"
                                                      (vars_to_list _1 _3)
# 325 "parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.variable_declaration list))) = _menhir_stack in
            let _v : (Ast.variable_declaration list list) = 
# 238 "<standard.mly>"
    ( [ x ] )
# 349 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_parameter_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | VAR ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | BEGIN | FUNCTION | PROCEDURE ->
                _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_statement_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.statement list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.statement list))) = _menhir_stack in
                let _v : (Ast.statement) = 
# 106 "parser.mly"
                                                      ( STMTBlock(_2) )
# 407 "parser.ml"
                 in
                _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.statement))), _, (_2 : (Ast.statement list))) = _menhir_stack in
        let _v : (Ast.statement list) = 
# 119 "parser.mly"
                                                    ( _1::_2 )
# 429 "parser.ml"
         in
        _menhir_goto_statement_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, (_2 : (Ast.statement list))) = _menhir_stack in
                let _v : (Ast.statement) = 
# 101 "parser.mly"
                                   (STMTBlock(_2))
# 450 "parser.ml"
                 in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_3 : (Ast.statement)) = _v in
                let ((_menhir_stack, _menhir_s, (_1 : (Ast.variable_declaration list))), _, (_2 : (Ast.subprogram_declaration list))) = _menhir_stack in
                let _v : (Ast.block) = 
# 78 "parser.mly"
                                                                       (Block(_1, _2, _3))
# 459 "parser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                (match _menhir_s with
                | MenhirState42 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((((_menhir_stack, _menhir_s), _, (_2 : (Ast.ident))), _, (xs : (Ast.variable_declaration list list))), _, (_7 : (Ast.block))) = _menhir_stack in
                    let _v =
                      let _4 =
                        let xss = 
# 229 "<standard.mly>"
    ( xs )
# 472 "parser.ml"
                         in
                        
# 257 "<standard.mly>"
    ( List.flatten xss )
# 477 "parser.ml"
                        
                      in
                      (
# 164 "parser.mly"
                                           ( ProcedureDeclaration(_2,_4,_7) )
# 483 "parser.ml"
                       : (Ast.subprogram_declaration))
                    in
                    _menhir_goto_subprogram _menhir_env _menhir_stack _menhir_s _v
                | MenhirState51 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((((_menhir_stack, _menhir_s), _, (_2 : (Ast.ident))), _, (xs : (Ast.variable_declaration list list))), _, (_7 : (Ast.pascaltype))), _, (_9 : (Ast.block))) = _menhir_stack in
                    let _v =
                      let _4 =
                        let xss = 
# 229 "<standard.mly>"
    ( xs )
# 496 "parser.ml"
                         in
                        
# 257 "<standard.mly>"
    ( List.flatten xss )
# 501 "parser.ml"
                        
                      in
                      (
# 166 "parser.mly"
                                           ( FunctionDeclaration(_2,_4,_9, _7) )
# 507 "parser.ml"
                       : (Ast.subprogram_declaration))
                    in
                    _menhir_goto_subprogram _menhir_env _menhir_stack _menhir_s _v
                | MenhirState2 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | EOF ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, (_2 : (
# 15 "parser.mly"
       (string)
# 522 "parser.ml"
                        ))), _, (_3 : (Ast.block))) = _menhir_stack in
                        let _v : (Ast.program) = 
# 74 "parser.mly"
                                ( Program(_2, _3) )
# 527 "parser.ml"
                         in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_1 : (Ast.program)) = _v in
                        Obj.magic _1
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    _menhir_fail ())
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_stype : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.simpletype) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_9 : (Ast.simpletype)) = _v in
        let (((_menhir_stack, _menhir_s), (_3 : (
# 13 "parser.mly"
       (int)
# 566 "parser.ml"
        ))), (_6 : (
# 13 "parser.mly"
       (int)
# 570 "parser.ml"
        ))) = _menhir_stack in
        let _v : (Ast.pascaltype) = 
# 176 "parser.mly"
                                           (ArrayType(_3, _6, _9))
# 575 "parser.ml"
         in
        _menhir_goto_ptype _menhir_env _menhir_stack _menhir_s _v
    | MenhirState49 | MenhirState35 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Ast.simpletype)) = _v in
        let _v : (Ast.pascaltype) = 
# 175 "parser.mly"
            (SimpleType(_1))
# 585 "parser.ml"
         in
        _menhir_goto_ptype _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.statement) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState57 | MenhirState139 | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | FOR ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | IDE _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
        | IF ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | READ ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | WHILE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | WRITE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState139
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.statement))) = _menhir_stack in
            let _v : (Ast.statement list) = 
# 118 "parser.mly"
                                                    ( [_1] )
# 620 "parser.ml"
             in
            _menhir_goto_statement_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139)
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, (_2 : (Ast.ident))), _, (_4 : (Ast.exp))), _, (_6 : (Ast.exp))), _, (_8 : (Ast.statement))) = _menhir_stack in
        let _v : (Ast.statement) = 
# 107 "parser.mly"
                                                ( STMTFor(_2, _4, _6, _8, true) )
# 634 "parser.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, (_2 : (Ast.ident))), _, (_4 : (Ast.exp))), _, (_6 : (Ast.exp))), _, (_8 : (Ast.statement))) = _menhir_stack in
        let _v : (Ast.statement) = 
# 108 "parser.mly"
                                                    ( STMTFor(_2, _4, _6, _8, false) )
# 644 "parser.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | FOR ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _v
            | IF ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | READ ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | WHILE ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | WRITE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState152
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152)
        | BEGIN | END | FOR | IDE _ | IF | READ | WHILE | WRITE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Ast.exp))), _, (_4 : (Ast.statement))) = _menhir_stack in
            let _v : (Ast.statement) = 
# 110 "parser.mly"
                                                   ( STMTIf(_2,_4,None) )
# 681 "parser.ml"
             in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (_2 : (Ast.exp))), _, (_4 : (Ast.statement))), _, (_6 : (Ast.statement))) = _menhir_stack in
        let _v : (Ast.statement) = 
# 109 "parser.mly"
                                                   ( STMTIf(_2,_4,Some _6) )
# 697 "parser.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (_2 : (Ast.exp))), _, (_4 : (Ast.statement))) = _menhir_stack in
        let _v : (Ast.statement) = 
# 112 "parser.mly"
                                                   ( STMTWhile(_2,_4) )
# 707 "parser.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_variable : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.variable) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (_3 : (Ast.variable))) = _menhir_stack in
                let _v : (Ast.statement) = 
# 114 "parser.mly"
                                                          ( STMTRead(_3) )
# 735 "parser.ml"
                 in
                _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState57 | MenhirState111 | MenhirState152 | MenhirState123 | MenhirState149 | MenhirState130 | MenhirState139 | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | INT _v ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | LP ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | MINUS ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | PSTRING _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | IDE _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | INT _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | LP ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | NOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | PSTRING _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | TRUE ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | RP ->
            _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run109 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | INT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | LP ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | MINUS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109

and _menhir_run112 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LP ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDE _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run121 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | INT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | LP ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | MINUS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121

and _menhir_run124 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124

and _menhir_run131 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | FOR ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState131 _v
    | IF ->
        _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | READ ->
        _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | WHILE ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | WRITE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState131
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState131

and _menhir_goto_separated_nonempty_list_COMMA_exp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState142 | MenhirState59 | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.exp list)) = _v in
        let _v : (Ast.exp list) = 
# 141 "<standard.mly>"
    ( x )
# 955 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.exp list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.exp))) = _menhir_stack in
        let _v : (Ast.exp list) = 
# 240 "<standard.mly>"
    ( x :: xs )
# 966 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_exp_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | INT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LP ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | MINUS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | INT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | LP ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | MINUS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run79 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | INT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | LP ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | MINUS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | INT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | LP ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | MINUS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | INT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | LP ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | MINUS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | INT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | LP ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | MINUS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | INT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | LP ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | MINUS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run87 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | INT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | LP ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | MINUS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run89 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | INT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | LP ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | MINUS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run91 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | INT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | LP ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | MINUS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | INT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | LP ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | MINUS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run93 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | INT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | LP ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | MINUS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.ident))), _, (xs : (Ast.exp list))) = _menhir_stack in
            let _v =
              let _3 = 
# 229 "<standard.mly>"
    ( xs )
# 1302 "parser.ml"
               in
              (
# 146 "parser.mly"
                                           ( CALL(_1,_3) )
# 1307 "parser.ml"
               : (Ast.exp))
            in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (xs : (Ast.exp list))) = _menhir_stack in
                let _v =
                  let _3 = 
# 229 "<standard.mly>"
    ( xs )
# 1336 "parser.ml"
                   in
                  (
# 113 "parser.mly"
                                                                        ( STMTWrite(_3) )
# 1341 "parser.ml"
                   : (Ast.statement))
                in
                _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (_1 : (Ast.ident))), _, (xs : (Ast.exp list))) = _menhir_stack in
                let _v =
                  let _3 = 
# 229 "<standard.mly>"
    ( xs )
# 1376 "parser.ml"
                   in
                  (
# 111 "parser.mly"
                                                                        ( STMTSubprogramCall(_1,_3) )
# 1381 "parser.ml"
                   : (Ast.statement))
                in
                _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_parameter__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.variable_declaration list list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | VAR ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState42
                | BEGIN | FUNCTION | PROCEDURE ->
                    _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState42
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ARRAY ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState49
                | TBOOLEAN ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState49
                | TCHAR ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState49
                | TINT ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState49
                | TSTRING ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState49
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.simpletype) = 
# 181 "parser.mly"
              (TypeString)
# 1490 "parser.ml"
     in
    _menhir_goto_stype _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.simpletype) = 
# 179 "parser.mly"
           (TypeInteger)
# 1501 "parser.ml"
     in
    _menhir_goto_stype _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.simpletype) = 
# 182 "parser.mly"
            (TypeChar)
# 1512 "parser.ml"
     in
    _menhir_goto_stype _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.simpletype) = 
# 180 "parser.mly"
               (TypeBoolean)
# 1523 "parser.ml"
     in
    _menhir_goto_stype _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOT ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | DOT ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | INT _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = (_menhir_stack, _v) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | RS ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | OF ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | TBOOLEAN ->
                                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState20
                                | TCHAR ->
                                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState20
                                | TINT ->
                                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20
                                | TSTRING ->
                                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
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
                            let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
                    let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_opt_subprogram_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.subprogram_declaration list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | FOR ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | IDE _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | IF ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | READ ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | WHILE ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | WRITE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce65 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ident) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ident))) = _menhir_stack in
    let _v : (Ast.variable) = 
# 186 "parser.mly"
          (EntireVar(_1))
# 1699 "parser.ml"
     in
    _menhir_goto_variable _menhir_env _menhir_stack _menhir_s _v

and _menhir_run118 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ident) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | INT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | LP ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | MINUS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState118
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | RS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.ident))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 131 "parser.mly"
                     (Var(IndexedVar(_1,_3 )))
# 1768 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
        let _v : (Ast.exp) = 
# 135 "parser.mly"
                    (MUL(_1,_3))
# 1786 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | DOWNTO | EQUAL | GREATER | GREATEREQUAL | LESS | LESSEQUAL | MINUS | OR | PLUS | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 132 "parser.mly"
                   (SUM(_1,_3))
# 1806 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
        let _v : (Ast.exp) = 
# 137 "parser.mly"
                   (MOD(_1,_3))
# 1822 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
        let _v : (Ast.exp) = 
# 136 "parser.mly"
                       (DIV(_1,_3))
# 1832 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DO | DOWNTO | OR | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 145 "parser.mly"
                 (OR(_1,_3))
# 1868 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | DOWNTO | EQUAL | GREATER | GREATEREQUAL | LESS | LESSEQUAL | MINUS | OR | PLUS | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 133 "parser.mly"
                    (SUB(_1,_3))
# 1894 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | DOWNTO | OR | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 139 "parser.mly"
                        (LE(_1, _3))
# 1924 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | DOWNTO | OR | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 140 "parser.mly"
                   (LT(_1, _3))
# 1954 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | DOWNTO | OR | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 141 "parser.mly"
                           (GE(_1, _3))
# 1984 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | DOWNTO | OR | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 142 "parser.mly"
                      (GT(_1, _3))
# 2014 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | DOWNTO | OR | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 138 "parser.mly"
                    (Equ(_1, _3))
# 2044 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | DOWNTO | OR | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 144 "parser.mly"
                  (AND(_1,_3))
# 2084 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState142 | MenhirState59 | MenhirState100 | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | INT _v ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | LP ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | MINUS ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | PSTRING _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp list) = 
# 238 "<standard.mly>"
    ( [ x ] )
# 2153 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_exp_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 147 "parser.mly"
                            ( _2 )
# 2197 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | DOWNTO | EQUAL | GREATER | GREATEREQUAL | LESS | LESSEQUAL | MINUS | OR | PLUS | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 134 "parser.mly"
                (SUB(Integer(0),_2))
# 2225 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | DOWNTO | OR | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 143 "parser.mly"
              (NOT(_2))
# 2265 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | FOR ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
            | IF ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | READ ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | WHILE ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | WRITE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
        | EQUAL ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | RS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.ident))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.variable) = 
# 185 "parser.mly"
                    (IndexedVar(_1, _3))
# 2367 "parser.ml"
             in
            _menhir_goto_variable _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | FOR ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | IF ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | READ ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | WHILE ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | WRITE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | DOWNTO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
            | INT _v ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
            | LP ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | MINUS ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | PSTRING _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState147
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
        | EQUAL ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
            | INT _v ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
            | LP ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | MINUS ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | PSTRING _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | FOR ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
            | IF ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | READ ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | WHILE ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | WRITE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState130
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
        | EQUAL ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.variable))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.statement) = 
# 105 "parser.mly"
                                                          ( STMTAss(_1,_3) )
# 2614 "parser.ml"
             in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | FOR ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
            | IF ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | READ ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | WHILE ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | WRITE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149)
        | EQUAL ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | MODU ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.exp list) = 
# 139 "<standard.mly>"
    ( [] )
# 2691 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.exp) = 
# 128 "parser.mly"
           (B(true))
# 2702 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 15 "parser.mly"
       (string)
# 2709 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 15 "parser.mly"
       (string)
# 2717 "parser.ml"
    )) = _v in
    let _v : (Ast.exp) = 
# 127 "parser.mly"
              (PString(_1))
# 2722 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | INT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | LP ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | MINUS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | INT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LP ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | MINUS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | INT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | LP ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | MINUS ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "parser.mly"
       (int)
# 2810 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 13 "parser.mly"
       (int)
# 2818 "parser.ml"
    )) = _v in
    let _v : (Ast.exp) = 
# 126 "parser.mly"
                   (Integer(_1))
# 2823 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.exp) = 
# 129 "parser.mly"
            (B(false))
# 2834 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.variable_declaration list list) = 
# 139 "<standard.mly>"
    ( [] )
# 2843 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_parameter__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_ide_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ident list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | TBOOLEAN ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | TCHAR ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | TINT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | TSTRING ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.ident))), _, (xs : (Ast.ident list))) = _menhir_stack in
        let _v : (Ast.ident list) = 
# 240 "<standard.mly>"
    ( x :: xs )
# 2888 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ide_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState46 | MenhirState38 | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ARRAY ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | TBOOLEAN ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | TCHAR ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | TINT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | TSTRING ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_opt_variable_declaration_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.variable_declaration list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUNCTION ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | PROCEDURE ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState29 in
        let _v : (Ast.subprogram_declaration list) = 
# 153 "parser.mly"
                                                  ( [] )
# 2941 "parser.ml"
         in
        _menhir_goto_opt_subprogram_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (string)
# 2952 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 12 "parser.mly"
       (string)
# 2960 "parser.ml"
    )) = _v in
    let _v : (Ast.ident) = 
# 189 "parser.mly"
          (_1)
# 2965 "parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState46 | MenhirState32 | MenhirState38 | MenhirState3 | MenhirState26 | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.ident))) = _menhir_stack in
            let _v : (Ast.ident list) = 
# 238 "<standard.mly>"
    ( [ x ] )
# 2991 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_ide_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | RP ->
                _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | RP ->
                _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState147 | MenhirState142 | MenhirState133 | MenhirState128 | MenhirState126 | MenhirState121 | MenhirState118 | MenhirState109 | MenhirState59 | MenhirState62 | MenhirState63 | MenhirState100 | MenhirState95 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState70 | MenhirState68 | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | INT _v ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | LP ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | MINUS ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | PSTRING _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | RP ->
                _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
        | LS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | INT _v ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | LP ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | MINUS ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | PSTRING _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
        | AND | COMMA | DIVISION | DO | DOWNTO | EQUAL | GREATER | GREATEREQUAL | LESS | LESSEQUAL | MINUS | MODU | OR | PLUS | RP | RS | SEMICOLON | THEN | TIMES | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.ident))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 130 "parser.mly"
          (Var(EntireVar(_1)))
# 3111 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LS ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | INT _v ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | LP ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | MINUS ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | PSTRING _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState57 | MenhirState111 | MenhirState123 | MenhirState152 | MenhirState149 | MenhirState130 | MenhirState131 | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
            | INT _v ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
            | LP ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | MINUS ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | PSTRING _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | RP ->
                _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack) MenhirState142
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
        | LS ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState131 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.variable_declaration list) = 
# 84 "parser.mly"
                                                  ( [] )
# 3439 "parser.ml"
     in
    _menhir_goto_opt_variable_declaration_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

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

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PROGRAM ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PSTRING _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | VAR ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
            | BEGIN | FUNCTION | PROCEDURE ->
                _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState2
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
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
