
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
  | MenhirState69
  | MenhirState67
  | MenhirState65
  | MenhirState64
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
  | MenhirState47
  | MenhirState46
  | MenhirState45
  | MenhirState44
  | MenhirState43
  | MenhirState42
  | MenhirState41
  | MenhirState40
  | MenhirState38
  | MenhirState37
  | MenhirState35
  | MenhirState32
  | MenhirState27
  | MenhirState25
  | MenhirState22
  | MenhirState18
  | MenhirState16
  | MenhirState14
  | MenhirState12
  | MenhirState7
  | MenhirState3
  | MenhirState2
  | MenhirState0

# 1 "src/parser.mly"
  
open Lang

# 110 "src/parser.ml"

let rec _menhir_run12 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TBOOL ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | TINT ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | TUNIT ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
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
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | FIRST ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | FIX ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | FUN ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | LET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
        | MUL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 | MenhirState37 | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | EQ | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.typ) = 
# 70 "src/parser.mly"
                         ( TConv (t1, t2) )
# 208 "src/parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RARROW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | EQ | MUL | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.typ) = 
# 71 "src/parser.mly"
                         ( TPair (t1, t2) )
# 231 "src/parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
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
        | MUL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
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
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | TINT ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | TUNIT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
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
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
            | FIRST ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | FIX ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | FUN ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
            | LET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | TINT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | TUNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
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
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
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
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35
                | TINT ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState35
                | TUNIT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState35
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
        | MUL ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | RARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
            | FIRST ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | FIX ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | FUN ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
            | LET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | TBOOL ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | TINT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | TUNIT ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37)
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

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | FIRST ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | FIX ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | FUN ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | FIRST ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | FIX ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | FUN ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
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

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | FIRST ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FIX ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FUN ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (Lang.exp) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | FIRST ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | FIX ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | FUN ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
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
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | FIRST ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | FIX ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | FUN ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
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

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Lang.typ) = 
# 69 "src/parser.mly"
                         ( TUnit )
# 589 "src/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Lang.typ) = 
# 67 "src/parser.mly"
                         ( TInt )
# 601 "src/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Lang.typ) = 
# 68 "src/parser.mly"
                         ( TBool )
# 613 "src/parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Lang.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | BOOL _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | FIRST ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | FIX ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | FUN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | INT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | LEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | MUL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | SUB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Lang.exp) = 
# 62 "src/parser.mly"
                                                                             ( EFst e )
# 663 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | FIRST ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | FIX ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | FUN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | INT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | MUL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | ADD | COMMA | ELSE | EOF | IN | LEQ | RPAREN | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 57 "src/parser.mly"
                                                                             ( EOp (e1, OSub, e2) )
# 706 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | FIRST ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | FIX ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | FUN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | INT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | ADD | COMMA | DIV | ELSE | EOF | IN | LEQ | MUL | RPAREN | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 60 "src/parser.mly"
                                                                             ( EOp (e1, OMul, e2) )
# 745 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
    | MenhirState69 | MenhirState67 | MenhirState62 | MenhirState65 | MenhirState59 | MenhirState61 | MenhirState54 | MenhirState56 | MenhirState58 | MenhirState53 | MenhirState52 | MenhirState40 | MenhirState42 | MenhirState45 | MenhirState47 | MenhirState51 | MenhirState49 | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | BOOL _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | FIRST ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | FIX ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | FUN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | INT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | LEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | MUL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | SUB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _v : (Lang.exp) = 
# 61 "src/parser.mly"
                                                                             ( EApp (e1, e2) )
# 793 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | BOOL _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | FIRST ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | FIX ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | FUN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | INT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | MUL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | SUB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | COMMA | ELSE | EOF | IN | LEQ | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 56 "src/parser.mly"
                                                                             ( EOp (e1, OLEq, e2) )
# 840 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | FIRST ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | FIX ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | FUN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | INT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | ADD | COMMA | DIV | ELSE | EOF | IN | LEQ | MUL | RPAREN | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 59 "src/parser.mly"
                                                                             ( EOp (e1, ODiv, e2) )
# 879 "src/parser.ml"
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
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | FIRST ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | FIX ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | FUN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | INT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | MUL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | ADD | COMMA | ELSE | EOF | IN | LEQ | RPAREN | SUB | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Lang.exp) = 
# 58 "src/parser.mly"
                                                                             ( EOp (e1, OAdd, e2) )
# 922 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | BOOL _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | FIRST ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | FIX ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | FUN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | INT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | LEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | MUL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | SUB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (vr1 : (
# 7 "src/parser.mly"
       (string)
# 969 "src/parser.ml"
            ))), (vr2 : (
# 7 "src/parser.mly"
       (string)
# 973 "src/parser.ml"
            ))), _, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))), _, (e : (Lang.exp))) = _menhir_stack in
            let _10 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 54 "src/parser.mly"
                                                                             ( EFix (Var vr1, Var vr2, t1, t2, e) )
# 984 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | BOOL _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | FIRST ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | FIX ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | FUN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | INT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | LEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | MUL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | SUB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), (vr : (
# 7 "src/parser.mly"
       (string)
# 1031 "src/parser.ml"
            ))), _, (t1 : (Lang.typ))), _, (t2 : (Lang.typ))), _, (e : (Lang.exp))) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 53 "src/parser.mly"
                                                                             ( EFun (Var vr, t1, t2, e) )
# 1042 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | BOOL _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | FIRST ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | FIX ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | FUN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | INT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | LEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | MUL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | SUB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState54 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | FIRST ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | FIX ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | FUN ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | LET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
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
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | BOOL _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState56 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | FIRST ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | FIX ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | FUN ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | LET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
        | FIRST ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | FIX ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | FUN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | INT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | LEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | MUL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | SUB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
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
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | BOOL _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | FIRST ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | FIX ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | FUN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | INT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | LEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MUL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | SUB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))), _), _, (e3 : (Lang.exp))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 55 "src/parser.mly"
                                                                             ( EIf (e1, e2, e3) )
# 1233 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | BOOL _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | FIRST ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | FIX ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | FUN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState59 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | FIRST ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | FIX ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | FUN ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | LET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
        | INT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | LEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | MUL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | SUB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState59
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
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
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | BOOL _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | FIRST ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | FIX ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | FUN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | INT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | LEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MUL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | SUB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), (vr : (
# 7 "src/parser.mly"
       (string)
# 1350 "src/parser.ml"
            ))), _, (t : (Lang.typ))), _, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 52 "src/parser.mly"
                                                                             ( ELet (Var vr, t, e1, e2) )
# 1359 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | BOOL _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState62 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL _v ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
            | FIRST ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | FIX ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | FUN ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | IF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | INT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
            | LET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | SECOND ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | FIRST ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | FIX ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | FUN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | INT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | LEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | MUL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState62 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 64 "src/parser.mly"
                                                                             ( e )
# 1437 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | SUB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | BOOL _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | FIRST ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | FIX ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | FUN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | INT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | LEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | MUL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState65 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Lang.exp))), _), _, (e2 : (Lang.exp))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Lang.exp) = 
# 51 "src/parser.mly"
                                                                             ( EPair (e1, e2) )
# 1491 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | SUB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | BOOL _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | FIRST ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | FIX ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | FUN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | INT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | LEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MUL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | SUB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | COMMA | ELSE | EOF | IN | RPAREN | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Lang.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Lang.exp) = 
# 63 "src/parser.mly"
                                                                             ( ESnd e )
# 1546 "src/parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | BOOL _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState69 in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Lang.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 39 "src/parser.mly"
       (Lang.exp)
# 1573 "src/parser.ml"
            ) = 
# 44 "src/parser.mly"
                                                                             ( e )
# 1577 "src/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 39 "src/parser.mly"
       (Lang.exp)
# 1584 "src/parser.ml"
            )) = _v in
            Obj.magic _1
        | FIRST ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | FIX ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | FUN ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | IF ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | INT _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | LEQ ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MUL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | SECOND ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | SUB ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
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
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
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
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
# 1792 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (vr : (
# 7 "src/parser.mly"
       (string)
# 1800 "src/parser.ml"
    )) = _v in
    let _v : (Lang.exp) = 
# 49 "src/parser.mly"
                                                                             ( EVar (Var vr) )
# 1805 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
    | FIRST ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | FIX ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | FUN ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState2
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
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
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | FIRST ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | FIX ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | FUN ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | LET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState3 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Lang.exp) = 
# 50 "src/parser.mly"
                                                                             ( EUnit )
# 1873 "src/parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
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

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 5 "src/parser.mly"
       (int)
# 1928 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 5 "src/parser.mly"
       (int)
# 1936 "src/parser.ml"
    )) = _v in
    let _v : (Lang.exp) = 
# 47 "src/parser.mly"
                                                                             ( EInt n )
# 1941 "src/parser.ml"
     in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | FIRST ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | FIX ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | FUN ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | LET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                | TBOOL ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                | TINT ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                | TUNIT ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState22
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
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

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                    | TBOOL ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                    | TINT ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                    | TUNIT ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState32
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
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

and _menhir_run38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | FIRST ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | FIX ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | FUN ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | SECOND ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "src/parser.mly"
       (bool)
# 2124 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 6 "src/parser.mly"
       (bool)
# 2132 "src/parser.ml"
    )) = _v in
    let _v : (Lang.exp) = 
# 48 "src/parser.mly"
                                                                             ( EBool b )
# 2137 "src/parser.ml"
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
# 39 "src/parser.mly"
       (Lang.exp)
# 2156 "src/parser.ml"
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
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | FIRST ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FIX ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FUN ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
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
  


# 2199 "src/parser.ml"
