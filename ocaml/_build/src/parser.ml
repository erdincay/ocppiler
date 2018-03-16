
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | VAR of (
# 7 "src/parser.mly"
       (string)
# 11 "src/parser.ml"
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
# 32 "src/parser.ml"
  )
    | IN
    | IF
    | FUN
    | FIX
    | FIRST
    | EQ
    | EOF
    | ELSE
    | DIV
    | COMMA
    | COLON
    | BOOL of (
# 6 "src/parser.mly"
       (bool)
# 48 "src/parser.ml"
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
  | MenhirState84
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
  | MenhirState72
  | MenhirState70
  | MenhirState69
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
  | MenhirState48
  | MenhirState46
  | MenhirState45
  | MenhirState43
  | MenhirState40
  | MenhirState35
  | MenhirState34
  | MenhirState32
  | MenhirState29
  | MenhirState25
  | MenhirState23
  | MenhirState17
  | MenhirState15
  | MenhirState13
  | MenhirState12
  | MenhirState8
  | MenhirState4
  | MenhirState3
  | MenhirState2
  | MenhirState0

# 1 "src/parser.mly"
  
open Lang

# 127 "src/parser.ml"

let rec _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | BOOL _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | FIRST ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | FIX ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | FUN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | LANGL ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | LPAREN ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | RPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | TBOOL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | TINT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | TUNIT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run15 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LANGL ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | LPAREN ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | TBOOL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | TINT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | TUNIT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run17 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LANGL ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | LPAREN ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | TBOOL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | TINT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | TUNIT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lang.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RANGL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t : (Lang.typ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.typ) = 
# 85 "src/parser.mly"
                          ( TRef t )
# 235 "src/parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | RARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 | MenhirState45 | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | EQ | RANGL | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.typ) = 
# 83 "src/parser.mly"
                          ( TConv (t1, t2) )
# 262 "src/parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))) = _menhir_stack in
        let _2 = () in
        let _v : (Lang.typ) = 
# 84 "src/parser.mly"
                          ( TPair (t1, t2) )
# 279 "src/parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState35 | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t : (Lang.typ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.typ) = 
# 86 "src/parser.mly"
                          ( t )
# 301 "src/parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState8 ->
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
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | BOOL _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
            | FIRST ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | FIX ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | FUN ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | INT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | REF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
        | MUL ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | LPAREN ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | TBOOL ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | TINT ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                | TUNIT ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32
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
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | BOOL _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | FIRST ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | FIX ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | FUN ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | INT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | LANGL ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | LPAREN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | REF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | TBOOL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | TINT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | TUNIT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
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
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState43
                | LPAREN ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43
                | TBOOL ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState43
                | TINT ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState43
                | TUNIT ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState43
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
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
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | BOOL _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | FIRST ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | FIX ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | FUN ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | INT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | LANGL ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | LPAREN ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | REF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | TBOOL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | TINT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | TUNIT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
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

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | BOOL _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | FIRST ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | FIX ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | FUN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | BOOL _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | FIRST ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | FIX ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | FUN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | BOOL _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | FIRST ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | FIX ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | FUN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | BOOL _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | FIRST ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FIX ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FUN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | BOOL _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | FIRST ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | FIX ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | FUN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | BOOL _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | FIRST ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | FIX ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | FUN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | BOOL _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | FIRST ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | FIX ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | FUN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run5 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _2 = () in
    let _1 = () in
    let _v : (Lang.exp) = 
# 57 "src/parser.mly"
                                                                              ( EUnit )
# 819 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Lang.typ) = 
# 80 "src/parser.mly"
                          ( TUnit )
# 831 "src/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Lang.typ) = 
# 81 "src/parser.mly"
                          ( TInt )
# 843 "src/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Lang.typ) = 
# 82 "src/parser.mly"
                          ( TBool )
# 855 "src/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LANGL ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | LPAREN ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | TBOOL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | TINT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | TUNIT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LANGL ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | LPAREN ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TBOOL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TINT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TUNIT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lang.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | ASS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | DIV ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | MUL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | SEMI ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | SUB ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Lang.exp) = 
# 70 "src/parser.mly"
                                                                              ( EDeref e )
# 955 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | DIV ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | MUL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | ADD | ASS | COMMA | ELSE | EOF | IN | LEQ | RPAREN | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 72 "src/parser.mly"
                                                                              ( EOp (e1, OSub, e2) )
# 1002 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | ADD | ASS | COMMA | DIV | ELSE | EOF | IN | LEQ | MUL | RPAREN | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 75 "src/parser.mly"
                                                                              ( EOp (e1, OMul, e2) )
# 1045 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | MenhirState84 | MenhirState82 | MenhirState81 | MenhirState78 | MenhirState80 | MenhirState73 | MenhirState75 | MenhirState77 | MenhirState72 | MenhirState67 | MenhirState70 | MenhirState66 | MenhirState65 | MenhirState49 | MenhirState51 | MenhirState54 | MenhirState56 | MenhirState64 | MenhirState58 | MenhirState62 | MenhirState60 | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | ASS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | DIV ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | MUL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | SEMI ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | SUB ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _v : (Lang.exp) = 
# 76 "src/parser.mly"
                                                                              ( EApp (e1, e2) )
# 1101 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | ASS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | DIV ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | MUL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | SEMI ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | SUB ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 66 "src/parser.mly"
                                                                              ( ESeq (e1, e2) )
# 1158 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | DIV ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MUL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | SUB ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | ASS | COMMA | ELSE | EOF | IN | LEQ | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 71 "src/parser.mly"
                                                                              ( EOp (e1, OLEq, e2) )
# 1209 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | ADD | ASS | COMMA | DIV | ELSE | EOF | IN | LEQ | MUL | RPAREN | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 74 "src/parser.mly"
                                                                              ( EOp (e1, ODiv, e2) )
# 1252 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | DIV ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | MUL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | ADD | ASS | COMMA | ELSE | EOF | IN | LEQ | RPAREN | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 73 "src/parser.mly"
                                                                              ( EOp (e1, OAdd, e2) )
# 1299 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | ASS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | DIV ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MUL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | SUB ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | COMMA | ELSE | EOF | IN | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 68 "src/parser.mly"
                                                                              ( EAssign (e1, e2) )
# 1354 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | ADD | ASS | COMMA | DIV | ELSE | EOF | FIRST | IN | LEQ | MUL | RPAREN | SECOND | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Lang.exp) = 
# 77 "src/parser.mly"
                                                                              ( EFst e )
# 1393 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | ASS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | DIV ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MUL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | SEMI ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | SUB ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (vr1 : (
# 7 "src/parser.mly"
       (string)
# 1448 "src/parser.ml"
            ))), (vr2 : (
# 7 "src/parser.mly"
       (string)
# 1452 "src/parser.ml"
            ))), _, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))), _, (e : (Lang.exp))) = _menhir_stack in
            let _10 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 65 "src/parser.mly"
                                                                              ( EFix (Var vr1, Var vr2, t1, t2, e) )
# 1463 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState4 | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | ASS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState67 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | BOOL _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
            | FIRST ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | FIX ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | FUN ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | INT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | REF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState69
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
        | DIV ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MUL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState67 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 62 "src/parser.mly"
                                                                              ( e )
# 1551 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | SEMI ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | SUB ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | ASS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | DIV ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | MUL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState70 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 61 "src/parser.mly"
                                                                              ( EPair (e1, e2) )
# 1613 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | SEMI ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | SUB ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | ASS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | DIV ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | MUL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | SEMI ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | SUB ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (vr : (
# 7 "src/parser.mly"
       (string)
# 1676 "src/parser.ml"
            ))), _, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))), _, (e : (Lang.exp))) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 64 "src/parser.mly"
                                                                              ( EFun (Var vr, t1, t2, e) )
# 1687 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | ASS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | DIV ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MUL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | SEMI ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | SUB ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState73 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | BOOL _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
            | FIRST ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | FIX ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | FUN ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | INT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | REF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | ASS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | DIV ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState75 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | BOOL _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
            | FIRST ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | FIX ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | FUN ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | INT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | REF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MUL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SEMI ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SUB ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | ASS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | DIV ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | MUL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | SEMI ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | SUB ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState77
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))), _), _, (e3 : (Lang.exp))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 67 "src/parser.mly"
                                                                              ( EIf (e1, e2, e3) )
# 1910 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | ASS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | DIV ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState78 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | BOOL _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | FIRST ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | FIX ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | FUN ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | IF ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | INT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | REF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | MUL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | SEMI ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | SUB ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | ASS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | DIV ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MUL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SEMI ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SUB ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (vr : (
# 7 "src/parser.mly"
       (string)
# 2047 "src/parser.ml"
            ))), _, (t : (Lang.typ))), _, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 63 "src/parser.mly"
                                                                              ( ELet (Var vr, t, e1, e2) )
# 2056 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | ASS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | DIV ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MUL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SEMI ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SUB ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Lang.exp) = 
# 69 "src/parser.mly"
                                                                              ( ERef e )
# 2113 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | ADD | ASS | COMMA | DIV | ELSE | EOF | FIRST | IN | LEQ | MUL | RPAREN | SECOND | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Lang.exp) = 
# 78 "src/parser.mly"
                                                                              ( ESnd e )
# 2152 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | ASS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | BANG ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | BOOL _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | DIV ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState84 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 51 "src/parser.mly"
       (Lang.exp)
# 2183 "src/parser.ml"
            ) = 
# 55 "src/parser.mly"
                                                                              ( e )
# 2187 "src/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 51 "src/parser.mly"
       (Lang.exp)
# 2194 "src/parser.ml"
            )) = _v in
            Obj.magic _1
        | FIRST ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | FIX ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | FUN ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | IF ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | LEQ ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | MUL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | SEMI ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | SUB ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState84 ->
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
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
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
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
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "src/parser.mly"
       (string)
# 2450 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (vr : (
# 7 "src/parser.mly"
       (string)
# 2458 "src/parser.ml"
    )) = _v in
    let _v : (Lang.exp) = 
# 60 "src/parser.mly"
                                                                              ( EVar (Var vr) )
# 2463 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | BOOL _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | FIRST ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | FIX ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | FUN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | BOOL _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | FIRST ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | FIX ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | FUN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
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
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | BOOL _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | FIRST ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | FIX ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | FUN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | RPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | LPAREN ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | TBOOL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | TINT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | TUNIT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
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

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 5 "src/parser.mly"
       (int)
# 2621 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 5 "src/parser.mly"
       (int)
# 2629 "src/parser.ml"
    )) = _v in
    let _v : (Lang.exp) = 
# 58 "src/parser.mly"
                                                                              ( EInt n )
# 2634 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | BOOL _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | FIRST ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | FIX ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | FUN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | LPAREN ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | TBOOL ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | TINT ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | TUNIT ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
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

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                    | LPAREN ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                    | TBOOL ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                    | TINT ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                    | TUNIT ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
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

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | BOOL _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | FIRST ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FIX ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FUN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "src/parser.mly"
       (bool)
# 2833 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 6 "src/parser.mly"
       (bool)
# 2841 "src/parser.ml"
    )) = _v in
    let _v : (Lang.exp) = 
# 59 "src/parser.mly"
                                                                              ( EBool b )
# 2846 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | BOOL _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | FIRST ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | FIX ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | FUN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

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
# 51 "src/parser.mly"
       (Lang.exp)
# 2900 "src/parser.ml"
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
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | BOOL _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | FIRST ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FIX ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FUN ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IF ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 219 "/usr/share/menhir/standard.mly"
  


# 2947 "src/parser.ml"
