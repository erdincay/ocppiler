
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
    | GET
    | FUN
    | FIX
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
# 50 "src/parser.ml"
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
  | MenhirState95
  | MenhirState92
  | MenhirState91
  | MenhirState90
  | MenhirState89
  | MenhirState88
  | MenhirState87
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState77
  | MenhirState76
  | MenhirState73
  | MenhirState71
  | MenhirState70
  | MenhirState69
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
  | MenhirState51
  | MenhirState49
  | MenhirState46
  | MenhirState41
  | MenhirState40
  | MenhirState38
  | MenhirState35
  | MenhirState31
  | MenhirState29
  | MenhirState27
  | MenhirState22
  | MenhirState20
  | MenhirState15
  | MenhirState13
  | MenhirState12
  | MenhirState8
  | MenhirState4
  | MenhirState3
  | MenhirState1
  | MenhirState0

# 1 "src/parser.mly"
  
open Lang

# 133 "src/parser.ml"

let rec _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | BOOL _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | FIX ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | GET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LANGL ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LPAREN ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | RPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TBOOL ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TINT ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TUNIT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_goto_tupletyp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lang.typ list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (t1 : (Lang.typ))), _, (t2 : (Lang.typ list))) = _menhir_stack in
        let _2 = () in
        let _v : (Lang.typ list) = 
# 91 "src/parser.mly"
                                 ( t1::t2 )
# 192 "src/parser.ml"
         in
        _menhir_goto_tupletyp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (t1 : (Lang.typ))), _, (t2 : (Lang.typ list))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.typ) = 
# 86 "src/parser.mly"
                                          ( let t = t1::t2 in
                                             TTuple (t, List.length t) )
# 212 "src/parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

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

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lang.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RANGL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t : (Lang.typ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.typ) = 
# 88 "src/parser.mly"
                                          ( TRef t )
# 263 "src/parser.ml"
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
    | MenhirState40 | MenhirState51 | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | EQ | MUL | RANGL | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.typ) = 
# 85 "src/parser.mly"
                                          ( TConv (t1, t2) )
# 288 "src/parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState41 | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LANGL ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | LPAREN ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | TBOOL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | TINT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | TUNIT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
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
# 89 "src/parser.mly"
                                          ( t )
# 333 "src/parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState22 | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LANGL ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | LPAREN ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | TBOOL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | TINT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | TUNIT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
        | RARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (t : (Lang.typ))) = _menhir_stack in
            let _v : (Lang.typ list) = 
# 92 "src/parser.mly"
                                 ( t::[] )
# 374 "src/parser.ml"
             in
            _menhir_goto_tupletyp _menhir_env _menhir_stack _menhir_s _v
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
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | BOOL _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
            | FIX ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | GET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | IF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | INT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | REF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | VAR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
        | RARROW ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | LPAREN ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | TBOOL ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | TINT ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | TUNIT ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
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
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | BOOL _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | FIX ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | GET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | IF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | INT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | LANGL ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | LPAREN ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | REF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | TBOOL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | TINT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | TUNIT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | VAR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
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
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState49
                | LPAREN ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState49
                | TBOOL ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState49
                | TINT ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState49
                | TUNIT ->
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
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | BOOL _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | FIX ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | GET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | IF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | INT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | LANGL ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | LPAREN ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | REF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | TBOOL ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | TINT ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | TUNIT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState51
            | VAR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51
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

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_tuple : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lang.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp list))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 62 "src/parser.mly"
                                                                              ( let e = e1::e2 in
                                                                                ETuple (e, List.length e) )
# 648 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp list))) = _menhir_stack in
        let _2 = () in
        let _v : (Lang.exp list) = 
# 94 "src/parser.mly"
                             ( e1::e2 )
# 665 "src/parser.ml"
         in
        _menhir_goto_tuple _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | BOOL _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | FIX ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | GET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | BOOL _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | FIX ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | GET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | BOOL _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | FIX ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | GET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | BOOL _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | FIX ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | GET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | BOOL _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | FIX ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | GET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | BOOL _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | FIX ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | GET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | BOOL _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | FIX ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | GET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run5 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _2 = () in
    let _1 = () in
    let _v : (Lang.exp) = 
# 58 "src/parser.mly"
                                                                              ( EUnit )
# 926 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Lang.typ) = 
# 82 "src/parser.mly"
                                          ( TUnit )
# 938 "src/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Lang.typ) = 
# 83 "src/parser.mly"
                                          ( TInt )
# 950 "src/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Lang.typ) = 
# 84 "src/parser.mly"
                                          ( TBool )
# 962 "src/parser.ml"
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
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | ASS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | LEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | SEMI ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Lang.exp) = 
# 72 "src/parser.mly"
                                                                              ( EDeref e )
# 1062 "src/parser.ml"
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
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | ADD | ASS | COMMA | DO | ELSE | END | EOF | IN | LEQ | RPAREN | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 74 "src/parser.mly"
                                                                              ( EOp (e1, OSub, e2) )
# 1109 "src/parser.ml"
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
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | ADD | ASS | COMMA | DIV | DO | ELSE | END | EOF | IN | LEQ | MUL | RPAREN | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 77 "src/parser.mly"
                                                                              ( EOp (e1, OMul, e2) )
# 1152 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | MenhirState95 | MenhirState90 | MenhirState92 | MenhirState89 | MenhirState86 | MenhirState88 | MenhirState81 | MenhirState83 | MenhirState85 | MenhirState80 | MenhirState79 | MenhirState71 | MenhirState76 | MenhirState70 | MenhirState54 | MenhirState56 | MenhirState59 | MenhirState61 | MenhirState69 | MenhirState63 | MenhirState67 | MenhirState65 | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | ASS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | LEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | SEMI ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _v : (Lang.exp) = 
# 78 "src/parser.mly"
                                                                              ( EApp (e1, e2) )
# 1208 "src/parser.ml"
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
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | ASS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | LEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | SEMI ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 68 "src/parser.mly"
                                                                              ( ESeq (e1, e2) )
# 1265 "src/parser.ml"
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
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | ASS | COMMA | DO | ELSE | END | EOF | IN | LEQ | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 73 "src/parser.mly"
                                                                              ( EOp (e1, OLEq, e2) )
# 1316 "src/parser.ml"
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
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | ADD | ASS | COMMA | DIV | DO | ELSE | END | EOF | IN | LEQ | MUL | RPAREN | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 76 "src/parser.mly"
                                                                              ( EOp (e1, ODiv, e2) )
# 1359 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | ADD | ASS | COMMA | DO | ELSE | END | EOF | IN | LEQ | RPAREN | SEMI | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 75 "src/parser.mly"
                                                                              ( EOp (e1, OAdd, e2) )
# 1406 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | ASS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | LEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 70 "src/parser.mly"
                                                                              ( EAssign (e1, e2) )
# 1461 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | ASS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | LEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | SEMI ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (vr1 : (
# 7 "src/parser.mly"
       (string)
# 1516 "src/parser.ml"
            ))), (vr2 : (
# 7 "src/parser.mly"
       (string)
# 1520 "src/parser.ml"
            ))), _, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))), _, (e : (Lang.exp))) = _menhir_stack in
            let _10 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 67 "src/parser.mly"
                                                                              ( EFix (Var vr1, Var vr2, t1, t2, e) )
# 1531 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | MenhirState4 | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | ASS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState71 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | BOOL _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | FIX ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | GET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | IF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | INT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | REF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | VAR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | LEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState71 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 64 "src/parser.mly"
                                                                              ( e )
# 1619 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SEMI ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | MenhirState77 | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | ASS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState76 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | BOOL _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | FIX ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | GET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | IF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | INT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | REF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | VAR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | LEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | SEMI ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Lang.exp))) = _menhir_stack in
            let _v : (Lang.exp list) = 
# 95 "src/parser.mly"
                             ( e::[] )
# 1718 "src/parser.ml"
             in
            _menhir_goto_tuple _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | ASS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | LEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SEMI ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (vr : (
# 7 "src/parser.mly"
       (string)
# 1773 "src/parser.ml"
            ))), _, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))), _, (e : (Lang.exp))) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 66 "src/parser.mly"
                                                                              ( EFun (Var vr, t1, t2, e) )
# 1784 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | ASS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | LEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SEMI ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (n : (
# 5 "src/parser.mly"
       (int)
# 1839 "src/parser.ml"
            ))), _, (e : (Lang.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Lang.exp) = 
# 79 "src/parser.mly"
                                                                              ( EGet (n, e) )
# 1845 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | ASS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | LEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SEMI ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState81 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | BOOL _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | FIX ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | GET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | IF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | INT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | REF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | VAR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | ASS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState83 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | BOOL _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | FIX ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | GET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | IF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | INT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | REF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | VAR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState84
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | LEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | SEMI ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | ASS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | LEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | SEMI ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))), _), _, (e3 : (Lang.exp))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 69 "src/parser.mly"
                                                                              ( EIf (e1, e2, e3) )
# 2068 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | ASS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState86 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | BOOL _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | FIX ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | GET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | IF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | INT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | REF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | VAR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | LEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | SEMI ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | ASS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | LEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | SEMI ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (vr : (
# 7 "src/parser.mly"
       (string)
# 2205 "src/parser.ml"
            ))), _, (t : (Lang.typ))), _, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 65 "src/parser.mly"
                                                                              ( ELet (Var vr, t, e1, e2) )
# 2214 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | ASS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | LEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | SEMI ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | COMMA | DO | ELSE | END | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Lang.exp) = 
# 71 "src/parser.mly"
                                                                              ( ERef e )
# 2271 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | ASS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState90 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BANG ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | BOOL _v ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | FIX ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | GET ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | IF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | INT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | LPAREN ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | REF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | VAR _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | LEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | SEMI ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | ASS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState92 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 80 "src/parser.mly"
                                                                              ( EWhile (e1, e2) )
# 2387 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | LEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | SEMI ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | ASS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
        | DIV ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState95 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 52 "src/parser.mly"
       (Lang.exp)
# 2446 "src/parser.ml"
            ) = 
# 56 "src/parser.mly"
                                                                              ( e )
# 2450 "src/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 52 "src/parser.mly"
       (Lang.exp)
# 2457 "src/parser.ml"
            )) = _v in
            Obj.magic _1
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
        | LEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MUL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | SEMI ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | SUB ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
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
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | BOOL _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | FIX ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | GET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | REF ->
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
# 2764 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (vr : (
# 7 "src/parser.mly"
       (string)
# 2772 "src/parser.ml"
    )) = _v in
    let _v : (Lang.exp) = 
# 61 "src/parser.mly"
                                                                              ( EVar (Var vr) )
# 2777 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | BOOL _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | FIX ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | GET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | REF ->
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
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | BOOL _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | FIX ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | GET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | RPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4
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

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 5 "src/parser.mly"
       (int)
# 2900 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 5 "src/parser.mly"
       (int)
# 2908 "src/parser.ml"
    )) = _v in
    let _v : (Lang.exp) = 
# 59 "src/parser.mly"
                                                                              ( EInt n )
# 2913 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | BOOL _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | FIX ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | GET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BANG ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | BOOL _v ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
        | FIX ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | FUN ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
        | LET ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | LPAREN ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | REF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | VAR _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35
                | LPAREN ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
                | TBOOL ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35
                | TINT ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35
                | TUNIT ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState35
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
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

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                    | LPAREN ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                    | TBOOL ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                    | TINT ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                    | TUNIT ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
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

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "src/parser.mly"
       (bool)
# 3124 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 6 "src/parser.mly"
       (bool)
# 3132 "src/parser.ml"
    )) = _v in
    let _v : (Lang.exp) = 
# 60 "src/parser.mly"
                                                                              ( EBool b )
# 3137 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | BOOL _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | FIX ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | GET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | REF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | VAR _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

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
# 52 "src/parser.mly"
       (Lang.exp)
# 3191 "src/parser.ml"
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
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | BOOL _v ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | FIX ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | GET ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | REF ->
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
  


# 3238 "src/parser.ml"
