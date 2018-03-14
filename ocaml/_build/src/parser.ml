
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
    | SECOND
    | RPAREN
    | RARROW
    | MUL
    | LPAREN
    | LET
    | LEQ
    | INT of (
# 5 "src/parser.mly"
       (int)
# 28 "src/parser.ml"
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
# 44 "src/parser.ml"
  )
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
  | MenhirState75
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState65
  | MenhirState64
  | MenhirState62
  | MenhirState61
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
  | MenhirState47
  | MenhirState46
  | MenhirState43
  | MenhirState42
  | MenhirState40
  | MenhirState37
  | MenhirState32
  | MenhirState31
  | MenhirState29
  | MenhirState26
  | MenhirState22
  | MenhirState20
  | MenhirState16
  | MenhirState14
  | MenhirState11
  | MenhirState7
  | MenhirState3
  | MenhirState2
  | MenhirState0

# 1 "src/parser.mly"
  
open Lang

# 112 "src/parser.ml"

let rec _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | FIRST ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | FIX ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | FUN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | LET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LPAREN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | RPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | TINT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | TUNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TINT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TUNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lang.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState32 | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAREN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | TINT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | TUNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
        | RARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (t : (Lang.typ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.typ) = 
# 73 "src/parser.mly"
                                     ( t )
# 209 "src/parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 | MenhirState42 | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | EQ | MUL | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.typ) = 
# 71 "src/parser.mly"
                                     ( TConv (t1, t2) )
# 232 "src/parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.typ) = 
# 72 "src/parser.mly"
                                     ( TPair (t1, t2) )
# 259 "src/parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | FIRST ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | FIX ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | FUN ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | IF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | LET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState20
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20)
        | RARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
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
                | LPAREN ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | TINT ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | TUNIT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState29
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
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
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
            | FIRST ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | FIX ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | FUN ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | IF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
            | LET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | LPAREN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | TINT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | TUNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RARROW ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
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
                | LPAREN ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | TINT ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | TUNIT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40
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
        | RARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | FIRST ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | FIX ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | FUN ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | IF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | LET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | LPAREN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | TINT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | TUNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
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
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_fexp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lang.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (f : (Lang.exp)) = _v in
    let _v : (Lang.exp) = 
# 60 "src/parser.mly"
                                                                              ( f )
# 500 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | FIRST ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | FIX ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | FUN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | FIRST ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | FIX ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | FUN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | FIRST ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | FIX ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | FUN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | LET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | FIRST ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | FIX ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | FUN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | FIRST ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | FIX ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | FUN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | LET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run4 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _2 = () in
    let _1 = () in
    let _v : (Lang.exp) = 
# 50 "src/parser.mly"
                                                                              ( EUnit )
# 669 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Lang.typ) = 
# 70 "src/parser.mly"
                                     ( TUnit )
# 681 "src/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Lang.typ) = 
# 68 "src/parser.mly"
                                     ( TInt )
# 693 "src/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Lang.typ) = 
# 69 "src/parser.mly"
                                     ( TBool )
# 705 "src/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | TINT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | TUNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lang.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
        | FIX ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | FUN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
        | ADD | COMMA | DIV | ELSE | EOF | FIRST | IN | LEQ | MUL | RPAREN | SECOND | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Lang.exp) = 
# 57 "src/parser.mly"
                                                                              ( EFst e )
# 760 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
    | MenhirState75 | MenhirState73 | MenhirState70 | MenhirState72 | MenhirState65 | MenhirState67 | MenhirState69 | MenhirState64 | MenhirState59 | MenhirState62 | MenhirState58 | MenhirState47 | MenhirState55 | MenhirState57 | MenhirState49 | MenhirState53 | MenhirState51 | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | BOOL _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | FIRST ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | FIX ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | FUN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | LEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | MUL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | SUB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _v : (Lang.exp) = 
# 66 "src/parser.mly"
                                                                               ( EApp (e1, e2) )
# 808 "src/parser.ml"
             in
            _menhir_goto_fexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | FIRST ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | FIX ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | FUN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | MUL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | ADD | COMMA | ELSE | EOF | IN | LEQ | RPAREN | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 53 "src/parser.mly"
                                                                              ( EOp (e1, OSub, e2) )
# 851 "src/parser.ml"
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
        | BOOL _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | FIRST ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | FIX ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | FUN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | ADD | COMMA | DIV | ELSE | EOF | IN | LEQ | MUL | RPAREN | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 56 "src/parser.mly"
                                                                              ( EOp (e1, OMul, e2) )
# 890 "src/parser.ml"
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
        | BOOL _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | FIRST ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | FIX ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | FUN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | ADD | COMMA | DIV | ELSE | EOF | IN | LEQ | MUL | RPAREN | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 55 "src/parser.mly"
                                                                              ( EOp (e1, ODiv, e2) )
# 929 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | BOOL _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | FIRST ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | FIX ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | FUN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | MUL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | SUB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState55
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
        | COMMA | ELSE | EOF | IN | LEQ | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 52 "src/parser.mly"
                                                                              ( EOp (e1, OLEq, e2) )
# 976 "src/parser.ml"
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
        | BOOL _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | FIRST ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | FIX ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | FUN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | MUL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | ADD | COMMA | ELSE | EOF | IN | LEQ | RPAREN | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 54 "src/parser.mly"
                                                                              ( EOp (e1, OAdd, e2) )
# 1019 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | BOOL _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | FIRST ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | FIX ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | FUN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | LEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MUL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | SUB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (vr1 : (
# 7 "src/parser.mly"
       (string)
# 1066 "src/parser.ml"
            ))), (vr2 : (
# 7 "src/parser.mly"
       (string)
# 1070 "src/parser.ml"
            ))), _, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))), _, (e : (Lang.exp))) = _menhir_stack in
            let _10 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 64 "src/parser.mly"
                                                                               ( EFix (Var vr1, Var vr2, t1, t2, e) )
# 1081 "src/parser.ml"
             in
            _menhir_goto_fexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | MenhirState3 | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | BOOL _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState59 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | FIRST ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | FIX ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | FUN ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | IF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | LET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | FIRST ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | FIX ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | FUN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | LEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | MUL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState59 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 59 "src/parser.mly"
                                                                              ( e )
# 1159 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | SUB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | BOOL _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | FIRST ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | FIX ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | FUN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | LEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | MUL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState62 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 51 "src/parser.mly"
                                                                              ( EPair (e1, e2) )
# 1213 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | SUB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | BOOL _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | FIRST ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | FIX ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | FUN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | LEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MUL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | SUB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (vr : (
# 7 "src/parser.mly"
       (string)
# 1266 "src/parser.ml"
            ))), _, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))), _, (e : (Lang.exp))) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 63 "src/parser.mly"
                                                                               ( EFun (Var vr, t1, t2, e) )
# 1277 "src/parser.ml"
             in
            _menhir_goto_fexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | BOOL _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | FIRST ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | FIX ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | FUN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | LEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | MUL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | SUB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState65 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | FIRST ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | FIX ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | FUN ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | IF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | LET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | BOOL _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState67 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | FIRST ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | FIX ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | FUN ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | IF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | LET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
        | FIRST ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | FIX ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | FUN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | LEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MUL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | SUB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
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
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | BOOL _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | FIRST ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | FIX ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | FUN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | LEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MUL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | SUB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))), _), _, (e3 : (Lang.exp))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 65 "src/parser.mly"
                                                                               ( EIf (e1, e2, e3) )
# 1468 "src/parser.ml"
             in
            _menhir_goto_fexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | BOOL _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | FIRST ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | FIX ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | FUN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState70 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | FIRST ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | FIX ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | FUN ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | IF ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | INT _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | LET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | LEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | MUL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | SUB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | BOOL _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | FIRST ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | FIX ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | FUN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | LEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | MUL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | SUB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (vr : (
# 7 "src/parser.mly"
       (string)
# 1585 "src/parser.ml"
            ))), _, (t : (Lang.typ))), _, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 62 "src/parser.mly"
                                                                               ( ELet (Var vr, t, e1, e2) )
# 1594 "src/parser.ml"
             in
            _menhir_goto_fexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | FIX ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | FUN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | ADD | COMMA | DIV | ELSE | EOF | FIRST | IN | LEQ | MUL | RPAREN | SECOND | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Lang.exp) = 
# 58 "src/parser.mly"
                                                                              ( ESnd e )
# 1629 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | BOOL _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState75 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 41 "src/parser.mly"
       (Lang.exp)
# 1656 "src/parser.ml"
            ) = 
# 45 "src/parser.mly"
                                                                              ( e )
# 1660 "src/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 41 "src/parser.mly"
       (Lang.exp)
# 1667 "src/parser.ml"
            )) = _v in
            Obj.magic _1
        | FIRST ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | FIX ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | FUN ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | IF ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | INT _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | LEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | MUL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | SUB ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState75
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
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
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
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
# 1883 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (vr : (
# 7 "src/parser.mly"
       (string)
# 1891 "src/parser.ml"
    )) = _v in
    let _v : (Lang.exp) = 
# 49 "src/parser.mly"
                                                                              ( EVar (Var vr) )
# 1896 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | FIRST ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | FIX ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | FUN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | LET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | LPAREN ->
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
    | BOOL _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | FIRST ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | FIX ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | FUN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | LET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | RPAREN ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            | LPAREN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | TINT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | TUNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7)
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

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 5 "src/parser.mly"
       (int)
# 2009 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 5 "src/parser.mly"
       (int)
# 2017 "src/parser.ml"
    )) = _v in
    let _v : (Lang.exp) = 
# 47 "src/parser.mly"
                                                                              ( EInt n )
# 2022 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | FIRST ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | FIX ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | FUN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | LET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                | LPAREN ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | TINT ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | TUNIT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
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

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                    | LPAREN ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                    | TBOOL ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                    | TINT ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                    | TUNIT ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37)
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

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | FIRST ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | FIX ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | FUN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "src/parser.mly"
       (bool)
# 2209 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 6 "src/parser.mly"
       (bool)
# 2217 "src/parser.ml"
    )) = _v in
    let _v : (Lang.exp) = 
# 48 "src/parser.mly"
                                                                              ( EBool b )
# 2222 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

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
# 41 "src/parser.mly"
       (Lang.exp)
# 2241 "src/parser.ml"
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
    | BOOL _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | FIRST ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FIX ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FUN ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IF ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
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
  


# 2284 "src/parser.ml"
