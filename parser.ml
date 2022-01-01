
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
    | MINUS
    | LS
    | LP
    | LESSEQUAL
    | LESS
    | INT of (
# 13 "parser.mly"
       (int)
# 55 "parser.ml"
  )
    | IF
    | IDE of (
# 12 "parser.mly"
       (string)
# 61 "parser.ml"
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
  | MenhirState144
  | MenhirState138
  | MenhirState135
  | MenhirState129
  | MenhirState127
  | MenhirState126
  | MenhirState124
  | MenhirState122
  | MenhirState120
  | MenhirState119
  | MenhirState117
  | MenhirState114
  | MenhirState109
  | MenhirState107
  | MenhirState105
  | MenhirState97
  | MenhirState92
  | MenhirState90
  | MenhirState88
  | MenhirState86
  | MenhirState84
  | MenhirState82
  | MenhirState80
  | MenhirState78
  | MenhirState76
  | MenhirState74
  | MenhirState72
  | MenhirState69
  | MenhirState67
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


# 156 "parser.ml"

[@@@ocaml.warning "-4-39"]

let rec _menhir_goto_subprogram_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.subprogram_declaration list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Ast.subprogram_declaration list)) = _v in
        let _v : (Ast.subprogram_declaration list) = 
# 151 "parser.mly"
                                                        ( _1 )
# 170 "parser.ml"
         in
        _menhir_goto_opt_subprogram_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Ast.subprogram_declaration list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Ast.subprogram_declaration))) = _menhir_stack in
        let _v : (Ast.subprogram_declaration list) = 
# 156 "parser.mly"
                                                     ( _1::_2 )
# 181 "parser.ml"
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
# 197 "parser.ml"
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
# 208 "parser.ml"
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
# 225 "parser.ml"
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
# 236 "parser.ml"
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
# 155 "parser.mly"
                                                                 ( [_1] )
# 259 "parser.ml"
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
# 284 "parser.ml"
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
# 299 "parser.ml"
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
# 167 "parser.mly"
                                                      (vars_to_list _1 _3)
# 319 "parser.ml"
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
# 343 "parser.ml"
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
                _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) MenhirState51
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
    | MenhirState127 ->
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
# 401 "parser.ml"
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
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.statement))), _, (_2 : (Ast.statement list))) = _menhir_stack in
        let _v : (Ast.statement list) = 
# 118 "parser.mly"
                                                    ( _1::_2 )
# 423 "parser.ml"
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
# 444 "parser.ml"
                 in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_3 : (Ast.statement)) = _v in
                let ((_menhir_stack, _menhir_s, (_1 : (Ast.variable_declaration list))), _, (_2 : (Ast.subprogram_declaration list))) = _menhir_stack in
                let _v : (Ast.block) = 
# 78 "parser.mly"
                                                                       (Block(_1, _2, _3))
# 453 "parser.ml"
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
# 466 "parser.ml"
                         in
                        
# 257 "<standard.mly>"
    ( List.flatten xss )
# 471 "parser.ml"
                        
                      in
                      (
# 161 "parser.mly"
                                           ( ProcedureDeclaration(_2,_4,_7) )
# 477 "parser.ml"
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
# 490 "parser.ml"
                         in
                        
# 257 "<standard.mly>"
    ( List.flatten xss )
# 495 "parser.ml"
                        
                      in
                      (
# 163 "parser.mly"
                                           ( FunctionDeclaration(_2,_4,_9, _7) )
# 501 "parser.ml"
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
# 516 "parser.ml"
                        ))), _, (_3 : (Ast.block))) = _menhir_stack in
                        let _v : (Ast.program) = 
# 74 "parser.mly"
                                ( Program(_2, _3) )
# 521 "parser.ml"
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
# 560 "parser.ml"
        ))), (_6 : (
# 13 "parser.mly"
       (int)
# 564 "parser.ml"
        ))) = _menhir_stack in
        let _v : (Ast.pascaltype) = 
# 173 "parser.mly"
                                           (ArrayType(_3, _6, _9))
# 569 "parser.ml"
         in
        _menhir_goto_ptype _menhir_env _menhir_stack _menhir_s _v
    | MenhirState49 | MenhirState35 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Ast.simpletype)) = _v in
        let _v : (Ast.pascaltype) = 
# 172 "parser.mly"
            (SimpleType(_1))
# 579 "parser.ml"
         in
        _menhir_goto_ptype _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.statement) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState57 | MenhirState135 | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | FOR ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | IDE _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _v
        | IF ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | READ ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | WHILE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | WRITE ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState135
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.statement))) = _menhir_stack in
            let _v : (Ast.statement list) = 
# 117 "parser.mly"
                                                    ( [_1] )
# 614 "parser.ml"
             in
            _menhir_goto_statement_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, (_2 : (Ast.ident))), _, (_4 : (Ast.exp))), _, (_6 : (Ast.exp))), _, (_8 : (Ast.statement))) = _menhir_stack in
        let _v : (Ast.statement) = 
# 107 "parser.mly"
                                                ( STMTFor(_2, _4, _6, _8) )
# 628 "parser.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState119 ->
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
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | FOR ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
            | IF ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | READ ->
                _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | WHILE ->
                _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | WRITE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState144
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144)
        | BEGIN | END | FOR | IDE _ | IF | READ | WHILE | WRITE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Ast.exp))), _, (_4 : (Ast.statement))) = _menhir_stack in
            let _v : (Ast.statement) = 
# 109 "parser.mly"
                                                   ( STMTIf(_2,_4,None) )
# 665 "parser.ml"
             in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (_2 : (Ast.exp))), _, (_4 : (Ast.statement))), _, (_6 : (Ast.statement))) = _menhir_stack in
        let _v : (Ast.statement) = 
# 108 "parser.mly"
                                                   ( STMTIf(_2,_4,Some _6) )
# 681 "parser.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (_2 : (Ast.exp))), _, (_4 : (Ast.statement))) = _menhir_stack in
        let _v : (Ast.statement) = 
# 111 "parser.mly"
                                                   ( STMTWhile(_2,_4) )
# 691 "parser.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_variable : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.variable) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState109 ->
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
# 113 "parser.mly"
                                                          ( STMTRead(_3) )
# 719 "parser.ml"
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
    | MenhirState57 | MenhirState107 | MenhirState144 | MenhirState119 | MenhirState126 | MenhirState135 | MenhirState127 ->
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
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | INT _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | LP ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | PSTRING _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
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
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | IDE _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | INT _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | LP ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | NOT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | PSTRING _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | TRUE ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | RP ->
            _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack) MenhirState59
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

and _menhir_run105 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | INT _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | LP ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_run108 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run117 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | INT _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | LP ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117

and _menhir_run120 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120

and _menhir_run127 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | FOR ->
        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
    | IF ->
        _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | READ ->
        _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | WHILE ->
        _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | WRITE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127

and _menhir_goto_separated_nonempty_list_COMMA_exp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState138 | MenhirState59 | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.exp list)) = _v in
        let _v : (Ast.exp list) = 
# 141 "<standard.mly>"
    ( x )
# 931 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.exp list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.exp))) = _menhir_stack in
        let _v : (Ast.exp list) = 
# 240 "<standard.mly>"
    ( x :: xs )
# 942 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_exp_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | INT _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | LP ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | INT _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | LP ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run76 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | INT _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | LP ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState76
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_run78 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | INT _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | LP ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run80 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | INT _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | LP ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run82 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | INT _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | LP ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | INT _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | LP ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | INT _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | LP ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | INT _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | LP ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | INT _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | LP ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | INT _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | LP ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState92 ->
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
# 1230 "parser.ml"
               in
              (
# 143 "parser.mly"
                                           ( CALL(_1,_3) )
# 1235 "parser.ml"
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
# 1264 "parser.ml"
                   in
                  (
# 112 "parser.mly"
                                                                        ( STMTWrite(_3) )
# 1269 "parser.ml"
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
    | MenhirState138 ->
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
# 1304 "parser.ml"
                   in
                  (
# 110 "parser.mly"
                                                                        ( STMTSubprogramCall(_1,_3) )
# 1309 "parser.ml"
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
                    _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) MenhirState42
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
# 178 "parser.mly"
              (TypeString)
# 1418 "parser.ml"
     in
    _menhir_goto_stype _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.simpletype) = 
# 176 "parser.mly"
           (TypeInteger)
# 1429 "parser.ml"
     in
    _menhir_goto_stype _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.simpletype) = 
# 179 "parser.mly"
            (TypeChar)
# 1440 "parser.ml"
     in
    _menhir_goto_stype _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.simpletype) = 
# 177 "parser.mly"
               (TypeBoolean)
# 1451 "parser.ml"
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
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | FOR ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | IDE _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | IF ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | READ ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | WHILE ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState57
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

and _menhir_reduce62 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ident) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ident))) = _menhir_stack in
    let _v : (Ast.variable) = 
# 183 "parser.mly"
          (EntireVar(_1))
# 1627 "parser.ml"
     in
    _menhir_goto_variable _menhir_env _menhir_stack _menhir_s _v

and _menhir_run114 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ident) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
    | INT _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
    | LP ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | NOT ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | PSTRING _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
    | TRUE ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | RS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.ident))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 130 "parser.mly"
                     (Var(IndexedVar(_1,_3 )))
# 1692 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
        let _v : (Ast.exp) = 
# 133 "parser.mly"
                    (MUL(_1,_3))
# 1710 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | EQUAL | GREATER | GREATEREQUAL | LESS | LESSEQUAL | MINUS | OR | PLUS | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 131 "parser.mly"
                   (SUM(_1,_3))
# 1728 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
        let _v : (Ast.exp) = 
# 134 "parser.mly"
                       (DIV(_1,_3))
# 1744 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DO | OR | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 142 "parser.mly"
                 (OR(_1,_3))
# 1778 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | EQUAL | GREATER | GREATEREQUAL | LESS | LESSEQUAL | MINUS | OR | PLUS | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 132 "parser.mly"
                    (SUB(_1,_3))
# 1802 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | OR | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 136 "parser.mly"
                        (LE(_1, _3))
# 1830 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | OR | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 137 "parser.mly"
                   (LT(_1, _3))
# 1858 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | OR | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 138 "parser.mly"
                           (GE(_1, _3))
# 1886 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | OR | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 139 "parser.mly"
                      (GT(_1, _3))
# 1914 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | OR | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 135 "parser.mly"
                    (Equ(_1, _3))
# 1942 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | OR | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.exp))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 141 "parser.mly"
                  (AND(_1,_3))
# 1980 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState138 | MenhirState59 | MenhirState97 | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | INT _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | LP ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | PSTRING _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
        | DIVISION ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp list) = 
# 238 "<standard.mly>"
    ( [ x ] )
# 2045 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_exp_ _menhir_env _menhir_stack _menhir_s _v
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
        | AND ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 144 "parser.mly"
                            ( _2 )
# 2087 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | OR | RP | RS | SEMICOLON | THEN | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 140 "parser.mly"
              (NOT(_2))
# 2127 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | FOR ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | IF ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | READ ->
                _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | WHILE ->
                _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | WRITE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
        | EQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | RS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.ident))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.variable) = 
# 182 "parser.mly"
                    (IndexedVar(_1, _3))
# 2225 "parser.ml"
             in
            _menhir_goto_variable _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | FOR ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
            | IF ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | READ ->
                _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | WHILE ->
                _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | WRITE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState119
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
            | INT _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
            | LP ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | PSTRING _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState124
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
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
        | AND ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | FOR ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | IF ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | READ ->
                _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | WHILE ->
                _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | WRITE ->
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
        | EQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack)
        | DIVISION ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.variable))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.statement) = 
# 105 "parser.mly"
                                                          ( STMTAss(_1,_3) )
# 2437 "parser.ml"
             in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.exp list) = 
# 139 "<standard.mly>"
    ( [] )
# 2456 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.exp) = 
# 127 "parser.mly"
           (B(true))
# 2467 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 15 "parser.mly"
       (string)
# 2474 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 15 "parser.mly"
       (string)
# 2482 "parser.ml"
    )) = _v in
    let _v : (Ast.exp) = 
# 126 "parser.mly"
              (PString(_1))
# 2487 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | INT _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | LP ->
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
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | INT _v ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LP ->
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

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "parser.mly"
       (int)
# 2544 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 13 "parser.mly"
       (int)
# 2552 "parser.ml"
    )) = _v in
    let _v : (Ast.exp) = 
# 125 "parser.mly"
                   (Integer(_1))
# 2557 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.exp) = 
# 128 "parser.mly"
            (B(false))
# 2568 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.variable_declaration list list) = 
# 139 "<standard.mly>"
    ( [] )
# 2577 "parser.ml"
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
# 2622 "parser.ml"
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
# 150 "parser.mly"
                                                  ( [] )
# 2675 "parser.ml"
         in
        _menhir_goto_opt_subprogram_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (string)
# 2686 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 12 "parser.mly"
       (string)
# 2694 "parser.ml"
    )) = _v in
    let _v : (Ast.ident) = 
# 186 "parser.mly"
          (_1)
# 2699 "parser.ml"
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
# 2725 "parser.ml"
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
                _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack) MenhirState32
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
                _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack) MenhirState46
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
    | MenhirState138 | MenhirState129 | MenhirState124 | MenhirState122 | MenhirState117 | MenhirState114 | MenhirState105 | MenhirState59 | MenhirState62 | MenhirState97 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState86 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState72 | MenhirState69 | MenhirState67 | MenhirState63 ->
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
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | INT _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | LP ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | PSTRING _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | RP ->
                _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
        | LS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | INT _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | LP ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | PSTRING _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
        | AND | COMMA | DIVISION | DO | EQUAL | GREATER | GREATEREQUAL | LESS | LESSEQUAL | MINUS | OR | PLUS | RP | RS | SEMICOLON | THEN | TIMES | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.ident))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 129 "parser.mly"
          (Var(EntireVar(_1)))
# 2841 "parser.ml"
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
        | LS ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState120 ->
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
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | INT _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | LP ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | PSTRING _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState122
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState57 | MenhirState107 | MenhirState119 | MenhirState144 | MenhirState126 | MenhirState127 | MenhirState135 ->
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
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | INT _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | LP ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | NOT ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | PSTRING _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | TRUE ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | RP ->
                _menhir_reduce23 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
        | LS ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState135 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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

and _menhir_reduce29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.variable_declaration list) = 
# 84 "parser.mly"
                                                  ( [] )
# 3149 "parser.ml"
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
                _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) MenhirState2
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
