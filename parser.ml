
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
    | TREAL
    | TO
    | TINT
    | TIMES
    | THEN
    | TCHAR
    | TBOOLEAN
    | SEMICOLON
    | RS
    | RP
    | REAL of (
# 12 "parser.mly"
       (float)
# 29 "parser.ml"
  )
    | READ
    | QUOTE
    | PSTRING of (
# 14 "parser.mly"
       (string)
# 36 "parser.ml"
  )
    | PROGRAM
    | PROCEDURE
    | PLUS
    | PCHAR of (
# 15 "parser.mly"
       (char)
# 44 "parser.ml"
  )
    | PBOOLEAN of (
# 13 "parser.mly"
       (bool)
# 49 "parser.ml"
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
# 11 "parser.mly"
       (int)
# 62 "parser.ml"
  )
    | IF
    | IDE of (
# 10 "parser.mly"
       (string)
# 68 "parser.ml"
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
  | MenhirState149
  | MenhirState147
  | MenhirState144
  | MenhirState143
  | MenhirState141
  | MenhirState138
  | MenhirState136
  | MenhirState129
  | MenhirState123
  | MenhirState120
  | MenhirState114
  | MenhirState112
  | MenhirState111
  | MenhirState109
  | MenhirState107
  | MenhirState105
  | MenhirState104
  | MenhirState101
  | MenhirState97
  | MenhirState92
  | MenhirState90
  | MenhirState88
  | MenhirState84
  | MenhirState74
  | MenhirState72
  | MenhirState70
  | MenhirState68
  | MenhirState66
  | MenhirState63
  | MenhirState61
  | MenhirState59
  | MenhirState55
  | MenhirState54
  | MenhirState51
  | MenhirState49
  | MenhirState43
  | MenhirState42
  | MenhirState38
  | MenhirState36
  | MenhirState35
  | MenhirState34
  | MenhirState30
  | MenhirState27
  | MenhirState24
  | MenhirState22
  | MenhirState21
  | MenhirState18
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

let rec _menhir_goto_subprogram_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.subprogam_declaration list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Ast.subprogam_declaration list)) = _v in
        let _v : (Ast.subprogam_declaration list) = 
# 170 "parser.mly"
                                                        ( _1 )
# 176 "parser.ml"
         in
        _menhir_goto_opt_subprogram_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Ast.subprogam_declaration list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Ast.subprogam_declaration))) = _menhir_stack in
        let _v : (Ast.subprogam_declaration list) = 
# 175 "parser.mly"
                                                     ( _1::_2 )
# 187 "parser.ml"
         in
        _menhir_goto_subprogram_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_subprogram : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.subprogam_declaration) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUNCTION ->
        _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | PROCEDURE ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Ast.subprogam_declaration))) = _menhir_stack in
        let _v : (Ast.subprogam_declaration list) = 
# 174 "parser.mly"
                                                                 ( [_1] )
# 210 "parser.ml"
         in
        _menhir_goto_subprogram_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147

and _menhir_goto_statement_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.statement list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState112 ->
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
# 104 "parser.mly"
                                                      ( STMTBlock(_2) )
# 240 "parser.ml"
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
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.statement))), _, (_2 : (Ast.statement list))) = _menhir_stack in
        let _v : (Ast.statement list) = 
# 116 "parser.mly"
                                                    ( _1::_2 )
# 262 "parser.ml"
         in
        _menhir_goto_statement_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState36 ->
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
                let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.statement list))) = _menhir_stack in
                let _v : (Ast.statement) = 
# 99 "parser.mly"
                                   (STMTBlock(_2))
# 283 "parser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                (match _menhir_s with
                | MenhirState35 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((((_menhir_stack, _menhir_s), _, (_2 : (Ast.ident))), _, (xs : (Ast.variable_declaration list list))), _, (_7 : (Ast.variable_declaration list))), _, (_8 : (Ast.statement))) = _menhir_stack in
                    let _v =
                      let _4 =
                        let xss = 
# 229 "<standard.mly>"
    ( xs )
# 296 "parser.ml"
                         in
                        
# 257 "<standard.mly>"
    ( List.flatten xss )
# 301 "parser.ml"
                        
                      in
                      (
# 180 "parser.mly"
                                           ( ProcedureDeclaration(_2,_4,_7, _8) )
# 307 "parser.ml"
                       : (Ast.subprogam_declaration))
                    in
                    _menhir_goto_subprogram _menhir_env _menhir_stack _menhir_s _v
                | MenhirState144 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((((((_menhir_stack, _menhir_s), _, (_2 : (Ast.ident))), _, (xs : (Ast.variable_declaration list list))), _, (_7 : (Ast.pascaltype))), _, (_9 : (Ast.variable_declaration list))), _, (_10 : (Ast.statement))) = _menhir_stack in
                    let _v =
                      let _4 =
                        let xss = 
# 229 "<standard.mly>"
    ( xs )
# 320 "parser.ml"
                         in
                        
# 257 "<standard.mly>"
    ( List.flatten xss )
# 325 "parser.ml"
                        
                      in
                      (
# 182 "parser.mly"
                                           ( FunctionDeclaration(_2,_4,_9,_10, _7) )
# 331 "parser.ml"
                       : (Ast.subprogam_declaration))
                    in
                    _menhir_goto_subprogram _menhir_env _menhir_stack _menhir_s _v
                | MenhirState149 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | EOF ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((((_menhir_stack, (_2 : (
# 14 "parser.mly"
       (string)
# 346 "parser.ml"
                        ))), _, (_3 : (Ast.variable_declaration list))), _, (_4 : (Ast.subprogam_declaration list))), _, (_5 : (Ast.statement))) = _menhir_stack in
                        let _v : (Ast.program) = 
# 75 "parser.mly"
                                                                                           ( Program(_2, _3, _4, _5) )
# 351 "parser.ml"
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

and _menhir_goto_separated_nonempty_list_COMMA_parameter_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.variable_declaration list list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState138 | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.variable_declaration list list)) = _v in
        let _v : (Ast.variable_declaration list list) = 
# 141 "<standard.mly>"
    ( x )
# 390 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_parameter__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.variable_declaration list list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.variable_declaration list))) = _menhir_stack in
        let _v : (Ast.variable_declaration list list) = 
# 240 "<standard.mly>"
    ( x :: xs )
# 401 "parser.ml"
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
# 88 "parser.mly"
                                                ( _1@_2 )
# 418 "parser.ml"
         in
        _menhir_goto_variable_declaration_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Ast.variable_declaration list)) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Ast.variable_declaration list) = 
# 83 "parser.mly"
                                                      ( _2 )
# 429 "parser.ml"
         in
        _menhir_goto_opt_variable_declaration_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run58 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.arithexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.arithexp))) = _menhir_stack in
    let _v : (Ast.arithexp) = 
# 146 "parser.mly"
                                     ( _2 )
# 443 "parser.ml"
     in
    _menhir_goto_arithexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.arithexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | REAL _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.arithexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | REAL _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.arithexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | REAL _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.arithexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | REAL _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.arithexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | REAL _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.arithexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | REAL _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.arithexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | REAL _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run74 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.arithexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | REAL _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.arithexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | REAL _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.statement) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState36 | MenhirState120 | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | FOR ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | IDE _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
        | IF ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | READ ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | WHILE ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | WRITE ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.statement))) = _menhir_stack in
            let _v : (Ast.statement list) = 
# 115 "parser.mly"
                                                    ( [_1] )
# 638 "parser.ml"
             in
            _menhir_goto_statement_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, (_2 : (Ast.ident))), _, (_4 : (Ast.arithexp))), _, (_6 : (Ast.arithexp))), _, (_8 : (Ast.statement))) = _menhir_stack in
        let _v : (Ast.statement) = 
# 105 "parser.mly"
                                                          ( STMTFor(_2, _4, _6, _8) )
# 652 "parser.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState104 ->
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
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | FOR ->
                _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
            | IF ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | READ ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | WHILE ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | WRITE ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
        | BEGIN | END | FOR | IDE _ | IF | READ | WHILE | WRITE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_3 : (Ast.booleanexp))), _, (_6 : (Ast.statement))) = _menhir_stack in
            let _v : (Ast.statement) = 
# 107 "parser.mly"
                                                                ( STMTIf(_3,_6,None) )
# 689 "parser.ml"
             in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (_3 : (Ast.booleanexp))), _, (_6 : (Ast.statement))), _, (_8 : (Ast.statement))) = _menhir_stack in
        let _v : (Ast.statement) = 
# 106 "parser.mly"
                                                                ( STMTIf(_3,_6,Some _8) )
# 705 "parser.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (_2 : (Ast.booleanexp))), _, (_4 : (Ast.statement))) = _menhir_stack in
        let _v : (Ast.statement) = 
# 109 "parser.mly"
                                                          ( STMTWhile(_2,_4) )
# 715 "parser.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_exp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState123 | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.exp list)) = _v in
        let _v : (Ast.exp list) = 
# 141 "<standard.mly>"
    ( x )
# 731 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.exp list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.exp))) = _menhir_stack in
        let _v : (Ast.exp list) = 
# 240 "<standard.mly>"
    ( x :: xs )
# 742 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_exp_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_stype : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.simpletype) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Ast.simpletype)) = _v in
    let _v : (Ast.pascaltype) = 
# 191 "parser.mly"
            (Simpletype(_1))
# 756 "parser.ml"
     in
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
# 92 "parser.mly"
                                                              (vars_to_list _1 _3)
# 773 "parser.ml"
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
# 87 "parser.mly"
                                         ( _1 )
# 788 "parser.ml"
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
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.ident list))), _, (_3 : (Ast.pascaltype))) = _menhir_stack in
        let _v : (Ast.variable_declaration list) = 
# 186 "parser.mly"
                                                      (vars_to_list _1 _3)
# 808 "parser.ml"
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
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.variable_declaration list))) = _menhir_stack in
            let _v : (Ast.variable_declaration list list) = 
# 238 "<standard.mly>"
    ( [ x ] )
# 832 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_parameter_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState141 ->
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
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | BEGIN ->
                _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState38 ->
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
# 891 "parser.ml"
                   in
                  (
# 110 "parser.mly"
                                                                        ( STMTWrite(_3) )
# 896 "parser.ml"
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
    | MenhirState123 ->
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
# 931 "parser.ml"
                   in
                  (
# 108 "parser.mly"
                                                                        ( STMTSubprogramCall(_1,_3) )
# 936 "parser.ml"
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

and _menhir_goto_variable : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.variable) -> 'ttv_return =
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
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (_3 : (Ast.variable))) = _menhir_stack in
                let _v : (Ast.statement) = 
# 111 "parser.mly"
                                                          ( STMTRead(_3) )
# 977 "parser.ml"
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
    | MenhirState36 | MenhirState90 | MenhirState129 | MenhirState104 | MenhirState111 | MenhirState120 | MenhirState112 ->
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
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | LP ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | NOT ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | PSTRING _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | REAL _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_arithexp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.arithexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState88 | MenhirState101 | MenhirState42 | MenhirState49 | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
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
        | DIVISION ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | EQUAL | GREATER | GREATEREQUAL | LESS | LESSEQUAL | MINUS | OR | PLUS | RP | RS | SEMICOLON | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
            let _v : (Ast.arithexp) = 
# 142 "parser.mly"
                             (SUM(_1,_3))
# 1100 "parser.ml"
             in
            _menhir_goto_arithexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
        let _v : (Ast.arithexp) = 
# 145 "parser.mly"
                                 (DIV(_1,_3))
# 1116 "parser.ml"
         in
        _menhir_goto_arithexp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | EQUAL | GREATER | GREATEREQUAL | LESS | LESSEQUAL | MINUS | OR | PLUS | RP | RS | SEMICOLON | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
            let _v : (Ast.arithexp) = 
# 143 "parser.mly"
                              (SUB(_1,_3))
# 1134 "parser.ml"
             in
            _menhir_goto_arithexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
        let _v : (Ast.arithexp) = 
# 144 "parser.mly"
                              (MUL(_1,_3))
# 1150 "parser.ml"
         in
        _menhir_goto_arithexp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | OR | RP | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
            let _v : (Ast.booleanexp) = 
# 153 "parser.mly"
                                  (LE(_1, _3))
# 1172 "parser.ml"
             in
            _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | OR | RP | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
            let _v : (Ast.booleanexp) = 
# 154 "parser.mly"
                             (LT(_1, _3))
# 1200 "parser.ml"
             in
            _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | OR | RP | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
            let _v : (Ast.booleanexp) = 
# 155 "parser.mly"
                                     (GE(_1, _3))
# 1228 "parser.ml"
             in
            _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | OR | RP | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
            let _v : (Ast.booleanexp) = 
# 156 "parser.mly"
                                (GT(_1, _3))
# 1256 "parser.ml"
             in
            _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | OR | RP | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
            let _v : (Ast.booleanexp) = 
# 152 "parser.mly"
                              (Equ(_1, _3))
# 1284 "parser.ml"
             in
            _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState123 | MenhirState114 | MenhirState38 | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RP | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 127 "parser.mly"
                    (ArithExp(_1))
# 1353 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | RS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.ident))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
            let _v : (Ast.variable) = 
# 202 "parser.mly"
                         (IndexedVar(_1, _3))
# 1381 "parser.ml"
             in
            _menhir_goto_variable _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | LP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | REAL _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
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
        | DIVISION ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | FOR ->
                _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
            | IF ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | READ ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | WHILE ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | WRITE ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
        | MINUS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | IDE _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | INT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | LP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | NOT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | PSTRING _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | REAL _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | RP ->
            _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run88 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | LP ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | REAL _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run91 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run100 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | IDE _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | INT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | LP ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | NOT ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | REAL _v ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | TRUE ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
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
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_run112 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | FOR ->
        _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
    | IF ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | READ ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | WHILE ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | WRITE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState123 | MenhirState84 | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | LP ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | NOT ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | PSTRING _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | REAL _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp list) = 
# 238 "<standard.mly>"
    ( [ x ] )
# 1676 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_exp_ _menhir_env _menhir_stack _menhir_s _v
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
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.variable))), _, (_3 : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.statement) = 
# 103 "parser.mly"
                                                          ( STMTAss(_1,_3) )
# 1698 "parser.ml"
             in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.booleanexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LP ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | REAL _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.booleanexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | LP ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | REAL _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_goto_loption_separated_nonempty_list_COMMA_parameter__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.variable_declaration list list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState24 ->
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
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                | BEGIN ->
                    _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
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
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | TBOOLEAN ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState141
                | TCHAR ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState141
                | TINT ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState141
                | TREAL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState141
                | TSTRING ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState141
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
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
# 198 "parser.mly"
              (TypeString)
# 1848 "parser.ml"
     in
    _menhir_goto_stype _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.simpletype) = 
# 196 "parser.mly"
            (TypeReal)
# 1859 "parser.ml"
     in
    _menhir_goto_stype _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.simpletype) = 
# 195 "parser.mly"
           (TypeInteger)
# 1870 "parser.ml"
     in
    _menhir_goto_stype _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.simpletype) = 
# 199 "parser.mly"
            (TypeChar)
# 1881 "parser.ml"
     in
    _menhir_goto_stype _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.simpletype) = 
# 197 "parser.mly"
               (TypeBoolean)
# 1892 "parser.ml"
     in
    _menhir_goto_stype _menhir_env _menhir_stack _menhir_s _v

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | FOR ->
        _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | IF ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | READ ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | WHILE ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | WRITE ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_goto_opt_subprogram_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.subprogam_declaration list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run136 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.exp list) = 
# 139 "<standard.mly>"
    ( [] )
# 1971 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.booleanexp) = 
# 149 "parser.mly"
           (B(true))
# 1982 "parser.ml"
     in
    _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 14 "parser.mly"
       (string)
# 1989 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 14 "parser.mly"
       (string)
# 1997 "parser.ml"
    )) = _v in
    let _v : (Ast.miscexp) = 
# 163 "parser.mly"
              (PString(_1))
# 2002 "parser.ml"
     in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Ast.miscexp)) = _v in
    let _v : (Ast.exp) = 
# 129 "parser.mly"
                    (MiscExp(_1))
# 2010 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LP ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | REAL _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LP ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NOT ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | REAL _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | TRUE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.booleanexp) = 
# 150 "parser.mly"
            (B(false))
# 2071 "parser.ml"
     in
    _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (float)
# 2078 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 12 "parser.mly"
       (float)
# 2086 "parser.ml"
    )) = _v in
    let _v : (Ast.arithexp) = 
# 140 "parser.mly"
                   (Real(_1))
# 2091 "parser.ml"
     in
    _menhir_goto_arithexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | REAL _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (int)
# 2117 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 11 "parser.mly"
       (int)
# 2125 "parser.ml"
    )) = _v in
    let _v : (Ast.arithexp) = 
# 139 "parser.mly"
                   (Integer(_1))
# 2130 "parser.ml"
     in
    _menhir_goto_arithexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce65 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ident) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ident))) = _menhir_stack in
    let _v : (Ast.variable) = 
# 203 "parser.mly"
          (EntireVar(_1))
# 2140 "parser.ml"
     in
    _menhir_goto_variable _menhir_env _menhir_stack _menhir_s _v

and _menhir_run97 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ident) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | LP ->
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | REAL _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_reduce3 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ident) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ident))) = _menhir_stack in
    let _v : (Ast.arithexp) = 
# 141 "parser.mly"
          (NumVar(_1))
# 2168 "parser.ml"
     in
    _menhir_goto_arithexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_booleanexp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.booleanexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.booleanexp))) = _menhir_stack in
            let _v : (Ast.booleanexp) = 
# 160 "parser.mly"
                                   ( _2 )
# 2193 "parser.ml"
             in
            _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v
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
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DO | OR | RP | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.booleanexp))), _, (_3 : (Ast.booleanexp))) = _menhir_stack in
            let _v : (Ast.booleanexp) = 
# 159 "parser.mly"
                               (OR(_1,_3))
# 2215 "parser.ml"
             in
            _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.booleanexp))), _, (_3 : (Ast.booleanexp))) = _menhir_stack in
        let _v : (Ast.booleanexp) = 
# 158 "parser.mly"
                                (AND(_1,_3))
# 2231 "parser.ml"
         in
        _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.booleanexp))) = _menhir_stack in
        let _v : (Ast.booleanexp) = 
# 157 "parser.mly"
                     (NOT(_2))
# 2241 "parser.ml"
         in
        _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState123 | MenhirState114 | MenhirState38 | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RP | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.booleanexp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 128 "parser.mly"
                    (BooleanExp(_1))
# 2259 "parser.ml"
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
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | FOR ->
                _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | IF ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | READ ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | WHILE ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | WRITE ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | THEN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState104
                | FOR ->
                    _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState104
                | IDE _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
                | IF ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState104
                | READ ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState104
                | WHILE ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState104
                | WRITE ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState104
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
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

and _menhir_reduce27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.variable_declaration list list) = 
# 139 "<standard.mly>"
    ( [] )
# 2363 "parser.ml"
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
            | TBOOLEAN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | TCHAR ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | TINT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | TREAL ->
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
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.ident))), _, (xs : (Ast.ident list))) = _menhir_stack in
        let _v : (Ast.ident list) = 
# 240 "<standard.mly>"
    ( x :: xs )
# 2408 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ide_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState138 | MenhirState30 | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TBOOLEAN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | TCHAR ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | TINT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | TREAL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | TSTRING ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
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
    match _menhir_s with
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FUNCTION ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | PROCEDURE ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState21 in
            let _v : (Ast.subprogam_declaration list) = 
# 169 "parser.mly"
                                                  ( [] )
# 2463 "parser.ml"
             in
            _menhir_goto_opt_subprogram_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState144
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144)
    | _ ->
        _menhir_fail ()

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "parser.mly"
       (string)
# 2498 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 10 "parser.mly"
       (string)
# 2506 "parser.ml"
    )) = _v in
    let _v : (Ast.ident) = 
# 206 "parser.mly"
          (Ident(_1))
# 2511 "parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState138 | MenhirState24 | MenhirState30 | MenhirState3 | MenhirState18 | MenhirState5 ->
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
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18)
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.ident))) = _menhir_stack in
            let _v : (Ast.ident list) = 
# 238 "<standard.mly>"
    ( [ x ] )
# 2537 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_ide_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState22 ->
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
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | RP ->
                _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState123 | MenhirState114 | MenhirState101 | MenhirState88 | MenhirState84 | MenhirState38 | MenhirState42 | MenhirState51 | MenhirState49 | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | DO | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.ident))) = _menhir_stack in
            let _v : (Ast.booleanexp) = 
# 151 "parser.mly"
          (BoolVar(_1))
# 2581 "parser.ml"
             in
            _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v
        | COMMA | DIVISION | EQUAL | GREATER | GREATEREQUAL | LESS | LESSEQUAL | MINUS | PLUS | RP | SEMICOLON | TIMES ->
            _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState109 | MenhirState107 | MenhirState97 | MenhirState74 | MenhirState72 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState54 | MenhirState63 | MenhirState61 | MenhirState59 | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LS ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack)
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
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | LP ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | REAL _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 | MenhirState90 | MenhirState104 | MenhirState129 | MenhirState111 | MenhirState112 | MenhirState120 ->
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
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | LP ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | NOT ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | PSTRING _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | REAL _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | TRUE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | RP ->
                _menhir_reduce25 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
        | LS ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState136 ->
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
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _v
            | RP ->
                _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState138
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
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
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
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
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
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
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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

and _menhir_reduce32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.variable_declaration list) = 
# 82 "parser.mly"
                                                  ( [] )
# 2919 "parser.ml"
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
                _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack) MenhirState2
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
