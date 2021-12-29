
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
# 13 "parser.mly"
       (string)
# 29 "parser.ml"
  )
    | PROGRAM
    | PROCEDURE
    | PLUS
    | PCHAR of (
# 14 "parser.mly"
       (char)
# 37 "parser.ml"
  )
    | PBOOLEAN of (
# 12 "parser.mly"
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
# 11 "parser.mly"
       (int)
# 55 "parser.ml"
  )
    | IF
    | IDE of (
# 10 "parser.mly"
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
  | MenhirState147
  | MenhirState145
  | MenhirState142
  | MenhirState141
  | MenhirState139
  | MenhirState136
  | MenhirState134
  | MenhirState127
  | MenhirState121
  | MenhirState118
  | MenhirState112
  | MenhirState110
  | MenhirState109
  | MenhirState107
  | MenhirState105
  | MenhirState103
  | MenhirState102
  | MenhirState99
  | MenhirState95
  | MenhirState90
  | MenhirState88
  | MenhirState86
  | MenhirState82
  | MenhirState72
  | MenhirState70
  | MenhirState68
  | MenhirState66
  | MenhirState64
  | MenhirState61
  | MenhirState59
  | MenhirState57
  | MenhirState53
  | MenhirState52
  | MenhirState49
  | MenhirState47
  | MenhirState41
  | MenhirState40
  | MenhirState37
  | MenhirState35
  | MenhirState34
  | MenhirState33
  | MenhirState29
  | MenhirState26
  | MenhirState23
  | MenhirState21
  | MenhirState20
  | MenhirState17
  | MenhirState8
  | MenhirState5
  | MenhirState3
  | MenhirState2

# 1 "parser.mly"
   (* HEADER *)

open Ast;;

let vars_to_list var_ides var_type =
    List.map (fun x -> VariableDeclaration(x, var_type)) var_ides


# 155 "parser.ml"

[@@@ocaml.warning "-4-39"]

let rec _menhir_goto_subprogram_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.subprogam_declaration list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Ast.subprogam_declaration list)) = _v in
        let _v : (Ast.subprogam_declaration list) = 
# 168 "parser.mly"
                                                        ( _1 )
# 169 "parser.ml"
         in
        _menhir_goto_opt_subprogram_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Ast.subprogam_declaration list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Ast.subprogam_declaration))) = _menhir_stack in
        let _v : (Ast.subprogam_declaration list) = 
# 173 "parser.mly"
                                                     ( _1::_2 )
# 180 "parser.ml"
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
        _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | PROCEDURE ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState145
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Ast.subprogam_declaration))) = _menhir_stack in
        let _v : (Ast.subprogam_declaration list) = 
# 172 "parser.mly"
                                                                 ( [_1] )
# 203 "parser.ml"
         in
        _menhir_goto_subprogram_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145

and _menhir_goto_statement_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.statement list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState110 ->
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
# 103 "parser.mly"
                                                      ( STMTBlock(_2) )
# 233 "parser.ml"
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
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.statement))), _, (_2 : (Ast.statement list))) = _menhir_stack in
        let _v : (Ast.statement list) = 
# 115 "parser.mly"
                                                    ( _1::_2 )
# 255 "parser.ml"
         in
        _menhir_goto_statement_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState35 ->
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
# 98 "parser.mly"
                                   (STMTBlock(_2))
# 276 "parser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                (match _menhir_s with
                | MenhirState34 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((((_menhir_stack, _menhir_s), _, (_2 : (Ast.ident))), _, (xs : (Ast.variable_declaration list list))), _, (_7 : (Ast.variable_declaration list))), _, (_8 : (Ast.statement))) = _menhir_stack in
                    let _v =
                      let _4 =
                        let xss = 
# 229 "<standard.mly>"
    ( xs )
# 289 "parser.ml"
                         in
                        
# 257 "<standard.mly>"
    ( List.flatten xss )
# 294 "parser.ml"
                        
                      in
                      (
# 178 "parser.mly"
                                           ( ProcedureDeclaration(_2,_4,_7, _8) )
# 300 "parser.ml"
                       : (Ast.subprogam_declaration))
                    in
                    _menhir_goto_subprogram _menhir_env _menhir_stack _menhir_s _v
                | MenhirState142 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((((((_menhir_stack, _menhir_s), _, (_2 : (Ast.ident))), _, (xs : (Ast.variable_declaration list list))), _, (_7 : (Ast.pascaltype))), _, (_9 : (Ast.variable_declaration list))), _, (_10 : (Ast.statement))) = _menhir_stack in
                    let _v =
                      let _4 =
                        let xss = 
# 229 "<standard.mly>"
    ( xs )
# 313 "parser.ml"
                         in
                        
# 257 "<standard.mly>"
    ( List.flatten xss )
# 318 "parser.ml"
                        
                      in
                      (
# 180 "parser.mly"
                                           ( FunctionDeclaration(_2,_4,_9,_10, _7) )
# 324 "parser.ml"
                       : (Ast.subprogam_declaration))
                    in
                    _menhir_goto_subprogram _menhir_env _menhir_stack _menhir_s _v
                | MenhirState147 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | EOF ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((((_menhir_stack, (_2 : (
# 13 "parser.mly"
       (string)
# 339 "parser.ml"
                        ))), _, (_3 : (Ast.variable_declaration list))), _, (_4 : (Ast.subprogam_declaration list))), _, (_5 : (Ast.statement))) = _menhir_stack in
                        let _v : (Ast.program) = 
# 74 "parser.mly"
                                                                                           ( Program(_2, _3, _4, _5) )
# 344 "parser.ml"
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
    | MenhirState136 | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.variable_declaration list list)) = _v in
        let _v : (Ast.variable_declaration list list) = 
# 141 "<standard.mly>"
    ( x )
# 383 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_parameter__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.variable_declaration list list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.variable_declaration list))) = _menhir_stack in
        let _v : (Ast.variable_declaration list list) = 
# 240 "<standard.mly>"
    ( x :: xs )
# 394 "parser.ml"
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
# 87 "parser.mly"
                                                ( _1@_2 )
# 411 "parser.ml"
         in
        _menhir_goto_variable_declaration_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Ast.variable_declaration list)) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Ast.variable_declaration list) = 
# 82 "parser.mly"
                                                      ( _2 )
# 422 "parser.ml"
         in
        _menhir_goto_opt_variable_declaration_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run56 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.arithexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.arithexp))) = _menhir_stack in
    let _v : (Ast.arithexp) = 
# 144 "parser.mly"
                                     ( _2 )
# 436 "parser.ml"
     in
    _menhir_goto_arithexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.arithexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.arithexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.arithexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.arithexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.arithexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState66
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
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState68
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
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState70
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
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.arithexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.statement) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState35 | MenhirState118 | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | FOR ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | IDE _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
        | IF ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | READ ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | WHILE ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | WRITE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.statement))) = _menhir_stack in
            let _v : (Ast.statement list) = 
# 114 "parser.mly"
                                                    ( [_1] )
# 613 "parser.ml"
             in
            _menhir_goto_statement_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, (_2 : (Ast.ident))), _, (_4 : (Ast.arithexp))), _, (_6 : (Ast.arithexp))), _, (_8 : (Ast.statement))) = _menhir_stack in
        let _v : (Ast.statement) = 
# 104 "parser.mly"
                                                          ( STMTFor(_2, _4, _6, _8) )
# 627 "parser.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState102 ->
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
                _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | FOR ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
            | IF ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | READ ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | WHILE ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | WRITE ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
        | BEGIN | END | FOR | IDE _ | IF | READ | WHILE | WRITE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_3 : (Ast.booleanexp))), _, (_6 : (Ast.statement))) = _menhir_stack in
            let _v : (Ast.statement) = 
# 106 "parser.mly"
                                                                ( STMTIf(_3,_6,None) )
# 664 "parser.ml"
             in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (_3 : (Ast.booleanexp))), _, (_6 : (Ast.statement))), _, (_8 : (Ast.statement))) = _menhir_stack in
        let _v : (Ast.statement) = 
# 105 "parser.mly"
                                                                ( STMTIf(_3,_6,Some _8) )
# 680 "parser.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (_2 : (Ast.booleanexp))), _, (_4 : (Ast.statement))) = _menhir_stack in
        let _v : (Ast.statement) = 
# 108 "parser.mly"
                                                          ( STMTWhile(_2,_4) )
# 690 "parser.ml"
         in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_exp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState121 | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.exp list)) = _v in
        let _v : (Ast.exp list) = 
# 141 "<standard.mly>"
    ( x )
# 706 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.exp list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.exp))) = _menhir_stack in
        let _v : (Ast.exp list) = 
# 240 "<standard.mly>"
    ( x :: xs )
# 717 "parser.ml"
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
# 189 "parser.mly"
            (Simpletype(_1))
# 731 "parser.ml"
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
# 91 "parser.mly"
                                                              (vars_to_list _1 _3)
# 748 "parser.ml"
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
# 86 "parser.mly"
                                         ( _1 )
# 763 "parser.ml"
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
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.ident list))), _, (_3 : (Ast.pascaltype))) = _menhir_stack in
        let _v : (Ast.variable_declaration list) = 
# 184 "parser.mly"
                                                      (vars_to_list _1 _3)
# 783 "parser.ml"
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
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.variable_declaration list))) = _menhir_stack in
            let _v : (Ast.variable_declaration list list) = 
# 238 "<standard.mly>"
    ( [ x ] )
# 807 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_parameter_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState139 ->
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
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | BEGIN ->
                _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState141
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
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState37 ->
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
# 866 "parser.ml"
                   in
                  (
# 109 "parser.mly"
                                                                        ( STMTWrite(_3) )
# 871 "parser.ml"
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
    | MenhirState121 ->
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
# 906 "parser.ml"
                   in
                  (
# 107 "parser.mly"
                                                                        ( STMTSubprogramCall(_1,_3) )
# 911 "parser.ml"
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
    | MenhirState90 ->
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
# 110 "parser.mly"
                                                          ( STMTRead(_3) )
# 952 "parser.ml"
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
    | MenhirState35 | MenhirState88 | MenhirState127 | MenhirState102 | MenhirState109 | MenhirState118 | MenhirState110 ->
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
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
            | INT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
            | LP ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | NOT ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | PSTRING _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
            | TRUE ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
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
    | MenhirState86 | MenhirState99 | MenhirState40 | MenhirState47 | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | EQUAL | GREATER | GREATEREQUAL | LESS | LESSEQUAL | MINUS | OR | PLUS | RP | RS | SEMICOLON | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
            let _v : (Ast.arithexp) = 
# 140 "parser.mly"
                             (SUM(_1,_3))
# 1073 "parser.ml"
             in
            _menhir_goto_arithexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
        let _v : (Ast.arithexp) = 
# 143 "parser.mly"
                                 (DIV(_1,_3))
# 1089 "parser.ml"
         in
        _menhir_goto_arithexp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | EQUAL | GREATER | GREATEREQUAL | LESS | LESSEQUAL | MINUS | OR | PLUS | RP | RS | SEMICOLON | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
            let _v : (Ast.arithexp) = 
# 141 "parser.mly"
                              (SUB(_1,_3))
# 1107 "parser.ml"
             in
            _menhir_goto_arithexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
        let _v : (Ast.arithexp) = 
# 142 "parser.mly"
                              (MUL(_1,_3))
# 1123 "parser.ml"
         in
        _menhir_goto_arithexp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | OR | RP | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
            let _v : (Ast.booleanexp) = 
# 151 "parser.mly"
                                  (LE(_1, _3))
# 1145 "parser.ml"
             in
            _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | OR | RP | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
            let _v : (Ast.booleanexp) = 
# 152 "parser.mly"
                             (LT(_1, _3))
# 1173 "parser.ml"
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
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | OR | RP | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
            let _v : (Ast.booleanexp) = 
# 153 "parser.mly"
                                     (GE(_1, _3))
# 1201 "parser.ml"
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
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | OR | RP | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
            let _v : (Ast.booleanexp) = 
# 154 "parser.mly"
                                (GT(_1, _3))
# 1229 "parser.ml"
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
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | OR | RP | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
            let _v : (Ast.booleanexp) = 
# 150 "parser.mly"
                              (Equ(_1, _3))
# 1257 "parser.ml"
             in
            _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState121 | MenhirState112 | MenhirState37 | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQUAL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RP | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.arithexp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 126 "parser.mly"
                    (ArithExp(_1))
# 1326 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | RS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.ident))), _, (_3 : (Ast.arithexp))) = _menhir_stack in
            let _v : (Ast.variable) = 
# 199 "parser.mly"
                         (IndexedVar(_1, _3))
# 1354 "parser.ml"
             in
            _menhir_goto_variable _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
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
        | DIVISION ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | INT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
            | LP ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState107
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
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISION ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | FOR ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | IF ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | READ ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | WHILE ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | WRITE ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
        | MINUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | IDE _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
        | INT _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
        | LP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | NOT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | PSTRING _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
        | TRUE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | RP ->
            _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run86 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | LP ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | NOT ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | TRUE ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_run89 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run98 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | IDE _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
        | INT _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
        | LP ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | NOT ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | TRUE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run103 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103

and _menhir_run110 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | FOR ->
        _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
    | IF ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | READ ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | WHILE ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | WRITE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState121 | MenhirState82 | MenhirState37 ->
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
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | INT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | LP ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | NOT ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | PSTRING _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | TRUE ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.exp))) = _menhir_stack in
            let _v : (Ast.exp list) = 
# 238 "<standard.mly>"
    ( [ x ] )
# 1639 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_exp_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState112 ->
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
# 102 "parser.mly"
                                                          ( STMTAss(_1,_3) )
# 1661 "parser.ml"
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

and _menhir_run47 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.booleanexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | LP ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NOT ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | TRUE ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.booleanexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LP ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NOT ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TRUE ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_goto_loption_separated_nonempty_list_COMMA_parameter__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.variable_declaration list list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState23 ->
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
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | BEGIN ->
                    _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
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
    | MenhirState136 ->
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
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState139
                | TCHAR ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState139
                | TINT ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState139
                | TSTRING ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState139
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139)
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
# 195 "parser.mly"
              (TypeString)
# 1805 "parser.ml"
     in
    _menhir_goto_stype _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.simpletype) = 
# 193 "parser.mly"
           (TypeInteger)
# 1816 "parser.ml"
     in
    _menhir_goto_stype _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.simpletype) = 
# 196 "parser.mly"
            (TypeChar)
# 1827 "parser.ml"
     in
    _menhir_goto_stype _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.simpletype) = 
# 194 "parser.mly"
               (TypeBoolean)
# 1838 "parser.ml"
     in
    _menhir_goto_stype _menhir_env _menhir_stack _menhir_s _v

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | FOR ->
        _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IF ->
        _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | READ ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | WHILE ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | WRITE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_goto_opt_subprogram_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.subprogam_declaration list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_run134 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.exp list) = 
# 139 "<standard.mly>"
    ( [] )
# 1917 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_exp__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.booleanexp) = 
# 147 "parser.mly"
           (B(true))
# 1928 "parser.ml"
     in
    _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "parser.mly"
       (string)
# 1935 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 13 "parser.mly"
       (string)
# 1943 "parser.ml"
    )) = _v in
    let _v : (Ast.miscexp) = 
# 161 "parser.mly"
              (PString(_1))
# 1948 "parser.ml"
     in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Ast.miscexp)) = _v in
    let _v : (Ast.exp) = 
# 128 "parser.mly"
                    (MiscExp(_1))
# 1956 "parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LP ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NOT ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | TRUE ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LP ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NOT ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TRUE ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.booleanexp) = 
# 148 "parser.mly"
            (B(false))
# 2013 "parser.ml"
     in
    _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (int)
# 2037 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 11 "parser.mly"
       (int)
# 2045 "parser.ml"
    )) = _v in
    let _v : (Ast.arithexp) = 
# 138 "parser.mly"
                   (Integer(_1))
# 2050 "parser.ml"
     in
    _menhir_goto_arithexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce63 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ident) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ident))) = _menhir_stack in
    let _v : (Ast.variable) = 
# 200 "parser.mly"
          (EntireVar(_1))
# 2060 "parser.ml"
     in
    _menhir_goto_variable _menhir_env _menhir_stack _menhir_s _v

and _menhir_run95 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ident) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | INT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | LP ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95

and _menhir_reduce2 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ident) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (Ast.ident))) = _menhir_stack in
    let _v : (Ast.arithexp) = 
# 139 "parser.mly"
          (NumVar(_1))
# 2086 "parser.ml"
     in
    _menhir_goto_arithexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_booleanexp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.booleanexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.booleanexp))) = _menhir_stack in
            let _v : (Ast.booleanexp) = 
# 158 "parser.mly"
                                   ( _2 )
# 2111 "parser.ml"
             in
            _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DO | OR | RP | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Ast.booleanexp))), _, (_3 : (Ast.booleanexp))) = _menhir_stack in
            let _v : (Ast.booleanexp) = 
# 157 "parser.mly"
                               (OR(_1,_3))
# 2133 "parser.ml"
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
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.booleanexp))), _, (_3 : (Ast.booleanexp))) = _menhir_stack in
        let _v : (Ast.booleanexp) = 
# 156 "parser.mly"
                                (AND(_1,_3))
# 2149 "parser.ml"
         in
        _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (_2 : (Ast.booleanexp))) = _menhir_stack in
        let _v : (Ast.booleanexp) = 
# 155 "parser.mly"
                     (NOT(_2))
# 2159 "parser.ml"
         in
        _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState121 | MenhirState112 | MenhirState37 | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RP | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.booleanexp))) = _menhir_stack in
            let _v : (Ast.exp) = 
# 127 "parser.mly"
                    (BooleanExp(_1))
# 2177 "parser.ml"
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
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | FOR ->
                _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | IF ->
                _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | READ ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | WHILE ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | WRITE ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
        | OR ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                | FOR ->
                    _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                | IDE _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
                | IF ->
                    _menhir_run98 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                | READ ->
                    _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                | WHILE ->
                    _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                | WRITE ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState102
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
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

and _menhir_reduce26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.variable_declaration list list) = 
# 139 "<standard.mly>"
    ( [] )
# 2281 "parser.ml"
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
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.ident))), _, (xs : (Ast.ident list))) = _menhir_stack in
        let _v : (Ast.ident list) = 
# 240 "<standard.mly>"
    ( x :: xs )
# 2324 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ide_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState136 | MenhirState29 | MenhirState23 ->
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
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | TCHAR ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | TINT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | TSTRING ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
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
            _menhir_run134 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | PROCEDURE ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState20 in
            let _v : (Ast.subprogam_declaration list) = 
# 167 "parser.mly"
                                                  ( [] )
# 2377 "parser.ml"
             in
            _menhir_goto_opt_subprogram_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState142
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
    | _ ->
        _menhir_fail ()

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "parser.mly"
       (string)
# 2412 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 10 "parser.mly"
       (string)
# 2420 "parser.ml"
    )) = _v in
    let _v : (Ast.ident) = 
# 203 "parser.mly"
          (Ident(_1))
# 2425 "parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState136 | MenhirState23 | MenhirState29 | MenhirState3 | MenhirState17 | MenhirState5 ->
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
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.ident))) = _menhir_stack in
            let _v : (Ast.ident list) = 
# 238 "<standard.mly>"
    ( [ x ] )
# 2451 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_ide_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState21 ->
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
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
            | RP ->
                _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState121 | MenhirState112 | MenhirState99 | MenhirState86 | MenhirState82 | MenhirState37 | MenhirState40 | MenhirState49 | MenhirState47 | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | DO | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.ident))) = _menhir_stack in
            let _v : (Ast.booleanexp) = 
# 149 "parser.mly"
          (BoolVar(_1))
# 2495 "parser.ml"
             in
            _menhir_goto_booleanexp _menhir_env _menhir_stack _menhir_s _v
        | COMMA | DIVISION | EQUAL | GREATER | GREATEREQUAL | LESS | LESSEQUAL | MINUS | PLUS | RP | SEMICOLON | TIMES ->
            _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState107 | MenhirState105 | MenhirState95 | MenhirState72 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState64 | MenhirState52 | MenhirState61 | MenhirState59 | MenhirState57 | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | RP ->
            _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState103 ->
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
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | INT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | LP ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState35 | MenhirState88 | MenhirState102 | MenhirState127 | MenhirState109 | MenhirState110 | MenhirState118 ->
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
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | IDE _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
            | INT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
            | LP ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | NOT ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | PSTRING _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
            | TRUE ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | RP ->
                _menhir_reduce24 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
        | LS ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState134 ->
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
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
            | RP ->
                _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState136
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
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
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
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
    | MenhirState64 ->
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
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
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

and _menhir_reduce31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.variable_declaration list) = 
# 81 "parser.mly"
                                                  ( [] )
# 2829 "parser.ml"
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
