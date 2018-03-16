
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | VAR of (
# 7 "src/parser.mly"
       (string)
# 12 "src/parser.ml"
  )
    | TUNIT
    | TINT
    | THEN
    | TBOOL
    | SUB
    | SEMI
    | SECOND
    | RPAREN
    | REF
    | RARROW
    | RANGL
    | MUL
    | LPAREN
    | LET
    | LEQ
    | LANGL
    | INT of (
# 5 "src/parser.mly"
       (int)
# 33 "src/parser.ml"
  )
    | IN
    | IF
    | FUN
    | FIX
    | FIRST
    | EQ
    | EOF
    | END
    | ELSE
    | DO
    | DIV
    | COMMA
    | COLON
    | BOOL of (
# 6 "src/parser.mly"
       (bool)
# 51 "src/parser.ml"
  )
    | BANG
    | ASS
    | ADD
  
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
  | MenhirState89
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState78
  | MenhirState77
  | MenhirState76
  | MenhirState75
  | MenhirState74
  | MenhirState73
  | MenhirState71
  | MenhirState70
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState55
  | MenhirState54
  | MenhirState53
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState49
  | MenhirState47
  | MenhirState46
  | MenhirState44
  | MenhirState41
  | MenhirState36
  | MenhirState35
  | MenhirState33
  | MenhirState30
  | MenhirState26
  | MenhirState24
  | MenhirState18
  | MenhirState16
  | MenhirState14
  | MenhirState13
  | MenhirState9
  | MenhirState5
  | MenhirState4
  | MenhirState3
  | MenhirState1
  | MenhirState0

# 1 "src/parser.mly"
  
open Lang

# 134 "src/parser.ml"

let rec _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | BOOL _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | FIRST ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | FIX ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LANGL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LPAREN ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | RPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | SECOND ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | TBOOL ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | TINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | TUNIT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run16 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LANGL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | TBOOL ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | TINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | TUNIT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LANGL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TBOOL ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TUNIT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lang.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RANGL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t : (Lang.typ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.typ) = 
# 89 "src/parser.mly"
                          ( TRef t )
# 244 "src/parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | RARROW ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState35 | MenhirState46 | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | EQ | RANGL | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.typ) = 
# 87 "src/parser.mly"
                          ( TConv (t1, t2) )
# 271 "src/parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))) = _menhir_stack in
        let _2 = () in
        let _v : (Lang.typ) = 
# 88 "src/parser.mly"
                          ( TPair (t1, t2) )
# 288 "src/parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState36 | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t : (Lang.typ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.typ) = 
# 90 "src/parser.mly"
                          ( t )
# 310 "src/parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | BOOL _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | FIRST ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | FIX ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | FUN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | REF ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | SECOND ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | VAR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
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
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LANGL ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | LPAREN ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | TBOOL ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | TINT ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState33
                | TUNIT ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState33
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
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | BOOL _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | FIRST ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | FIX ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | FUN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | LANGL ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | LPAREN ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | REF ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | SECOND ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | TBOOL ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | TINT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | TUNIT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | VAR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState35
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
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LANGL ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                | LPAREN ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                | TBOOL ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                | TINT ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                | TUNIT ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
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
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | BOOL _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | FIRST ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | FIX ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | FUN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | LANGL ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | LPAREN ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | REF ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | SECOND ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | TBOOL ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | TINT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | TUNIT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46
            | VAR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState46
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
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | BOOL _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | FIRST ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | FIX ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | SECOND ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | BOOL _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | FIRST ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | FIX ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | SECOND ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | BOOL _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | FIRST ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | FIX ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | SECOND ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | BOOL _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | FIRST ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | FIX ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | SECOND ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | BOOL _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | FIRST ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | FIX ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | SECOND ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | BOOL _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | FIRST ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | FIX ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | SECOND ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | BOOL _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | FIRST ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | FIX ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | SECOND ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run6 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _2 = () in
    let _1 = () in
    let _v : (Lang.exp) = 
# 60 "src/parser.mly"
                                                                              ( EUnit )
# 848 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Lang.typ) = 
# 84 "src/parser.mly"
                          ( TUnit )
# 860 "src/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Lang.typ) = 
# 85 "src/parser.mly"
                          ( TInt )
# 872 "src/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Lang.typ) = 
# 86 "src/parser.mly"
                          ( TBool )
# 884 "src/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LANGL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TBOOL ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TUNIT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LANGL ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TBOOL ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TINT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TUNIT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lang.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | ASS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | LEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | SEMI ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Lang.exp) = 
# 73 "src/parser.mly"
                                                                              ( EDeref e )
# 986 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | ADD | ASS | COMMA | DO | ELSE | END | EOF | IN | LEQ | RPAREN | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 75 "src/parser.mly"
                                                                              ( EOp (e1, OSub, e2) )
# 1035 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | ADD | ASS | COMMA | DIV | DO | ELSE | END | EOF | IN | LEQ | MUL | RPAREN | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 78 "src/parser.mly"
                                                                              ( EOp (e1, OMul, e2) )
# 1080 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState89 | MenhirState84 | MenhirState86 | MenhirState83 | MenhirState82 | MenhirState79 | MenhirState81 | MenhirState74 | MenhirState76 | MenhirState78 | MenhirState73 | MenhirState68 | MenhirState71 | MenhirState67 | MenhirState66 | MenhirState50 | MenhirState52 | MenhirState55 | MenhirState57 | MenhirState65 | MenhirState59 | MenhirState63 | MenhirState61 | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | ASS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | LEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | SEMI ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _v : (Lang.exp) = 
# 79 "src/parser.mly"
                                                                              ( EApp (e1, e2) )
# 1138 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | ASS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | LEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | SEMI ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 69 "src/parser.mly"
                                                                              ( ESeq (e1, e2) )
# 1197 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | ASS | COMMA | DO | ELSE | END | EOF | IN | LEQ | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 74 "src/parser.mly"
                                                                              ( EOp (e1, OLEq, e2) )
# 1250 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | ADD | ASS | COMMA | DIV | DO | ELSE | END | EOF | IN | LEQ | MUL | RPAREN | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 77 "src/parser.mly"
                                                                              ( EOp (e1, ODiv, e2) )
# 1295 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | ADD | ASS | COMMA | DO | ELSE | END | EOF | IN | LEQ | RPAREN | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 76 "src/parser.mly"
                                                                              ( EOp (e1, OAdd, e2) )
# 1344 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | ASS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | LEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 71 "src/parser.mly"
                                                                              ( EAssign (e1, e2) )
# 1401 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | ADD | ASS | COMMA | DIV | DO | ELSE | END | EOF | FIRST | IN | LEQ | MUL | RPAREN | SECOND | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Lang.exp) = 
# 80 "src/parser.mly"
                                                                              ( EFst e )
# 1442 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | ASS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | LEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | SEMI ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (vr1 : (
# 7 "src/parser.mly"
       (string)
# 1499 "src/parser.ml"
            ))), (vr2 : (
# 7 "src/parser.mly"
       (string)
# 1503 "src/parser.ml"
            ))), _, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))), _, (e : (Lang.exp))) = _menhir_stack in
            let _10 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 68 "src/parser.mly"
                                                                              ( EFix (Var vr1, Var vr2, t1, t2, e) )
# 1514 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState5 | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | ASS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState68 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | BOOL _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | FIRST ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | FIX ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | FUN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | REF ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | SECOND ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | VAR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | LEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState68 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 65 "src/parser.mly"
                                                                              ( e )
# 1604 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | SEMI ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | ASS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | LEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState71 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 64 "src/parser.mly"
                                                                              ( EPair (e1, e2) )
# 1668 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | SEMI ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | ASS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | LEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | SEMI ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (vr : (
# 7 "src/parser.mly"
       (string)
# 1735 "src/parser.ml"
            ))), _, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))), _, (e : (Lang.exp))) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 67 "src/parser.mly"
                                                                              ( EFun (Var vr, t1, t2, e) )
# 1746 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | ASS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | LEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | SEMI ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState74 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | BOOL _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | FIRST ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | FIX ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | FUN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | REF ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | SECOND ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | VAR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | ASS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState76 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | BOOL _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | FIRST ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | FIX ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | FUN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | REF ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | SECOND ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | VAR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | LEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | SEMI ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | ASS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | LEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | SEMI ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))), _), _, (e3 : (Lang.exp))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 70 "src/parser.mly"
                                                                              ( EIf (e1, e2, e3) )
# 1979 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | ASS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState79 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | BOOL _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | FIRST ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | FIX ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | FUN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | REF ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | SECOND ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | VAR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | LEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SEMI ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | ASS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | LEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SEMI ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (vr : (
# 7 "src/parser.mly"
       (string)
# 2122 "src/parser.ml"
            ))), _, (t : (Lang.typ))), _, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 66 "src/parser.mly"
                                                                              ( ELet (Var vr, t, e1, e2) )
# 2131 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | ASS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | LEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SEMI ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Lang.exp) = 
# 72 "src/parser.mly"
                                                                              ( ERef e )
# 2190 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | ADD | ASS | COMMA | DIV | DO | ELSE | END | EOF | FIRST | IN | LEQ | MUL | RPAREN | SECOND | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Lang.exp) = 
# 81 "src/parser.mly"
                                                                              ( ESnd e )
# 2231 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | ASS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState84 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | BOOL _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
            | FIRST ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | FIX ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | FUN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | REF ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | SECOND ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | VAR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | LEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | SEMI ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | ASS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState86 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 82 "src/parser.mly"
                                                                              ( EWhile (e1, e2) )
# 2351 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | LEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | SEMI ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | ASS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | BANG ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | BOOL _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | DIV ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState89 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 54 "src/parser.mly"
       (Lang.exp)
# 2412 "src/parser.ml"
            ) = 
# 58 "src/parser.mly"
                                                                              ( e )
# 2416 "src/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 54 "src/parser.mly"
       (Lang.exp)
# 2423 "src/parser.ml"
            )) = _v in
            Obj.magic _1
        | FIRST ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | FIX ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | LEQ ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LET ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MUL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | REF ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | SECOND ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | SEMI ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | SUB ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
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
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | BOOL _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | FIRST ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | FIX ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | SECOND ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "src/parser.mly"
       (string)
# 2734 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (vr : (
# 7 "src/parser.mly"
       (string)
# 2742 "src/parser.ml"
    )) = _v in
    let _v : (Lang.exp) = 
# 63 "src/parser.mly"
                                                                              ( EVar (Var vr) )
# 2747 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | BOOL _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | FIRST ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | FIX ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | SECOND ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | BOOL _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | FIRST ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | FIX ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | SECOND ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | BOOL _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | FIRST ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | FIX ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | RPAREN ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | SECOND ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LANGL ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | TBOOL ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | TINT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | TUNIT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
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

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 5 "src/parser.mly"
       (int)
# 2911 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 5 "src/parser.mly"
       (int)
# 2919 "src/parser.ml"
    )) = _v in
    let _v : (Lang.exp) = 
# 61 "src/parser.mly"
                                                                              ( EInt n )
# 2924 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | BOOL _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | FIRST ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | FIX ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | SECOND ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | VAR _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LANGL ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                | LPAREN ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                | TBOOL ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                | TINT ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                | TUNIT ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
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

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | VAR _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | COLON ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | LANGL ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                    | LPAREN ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                    | TBOOL ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                    | TINT ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                    | TUNIT ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
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
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | BOOL _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | FIRST ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | FIX ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | SECOND ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "src/parser.mly"
       (bool)
# 3127 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 6 "src/parser.mly"
       (bool)
# 3135 "src/parser.ml"
    )) = _v in
    let _v : (Lang.exp) = 
# 62 "src/parser.mly"
                                                                              ( EBool b )
# 3140 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | BOOL _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | FIRST ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | FIX ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | SECOND ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

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

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 54 "src/parser.mly"
       (Lang.exp)
# 3196 "src/parser.ml"
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
    | BANG ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | BOOL _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | FIRST ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FIX ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SECOND ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 219 "/usr/share/menhir/standard.mly"
  


# 3245 "src/parser.ml"
