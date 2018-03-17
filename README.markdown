# Ocppiler
A bare-bones compiler written in C++, flex, and Bison, as well as OCaml and Menhir; made by [@hadleyel](https://github.com/hadleyel "Hadley") to experiment with programming language implementation.

## Grammar
This will be frequently modified and added to.
```
e ::= (e) | n | b | e1 (+) e2 | if e1 then e2 else e3
    | x | let x : t = e1 in e2
    | e1 e2 | fun (x:t1) : t2 -> e | fix f (x:t1) : t2 -> e
    | ()
    | (e1, e2, ...) | get n e
    | ref e | e1 := e2 | !e | e1 ; e2

t ::= int | bool | t1 -> t2 | unit | <t>
```
## Compiler
### Dependencies
#### For both
- [Git](https://git-scm.com/)
- [Make](https://www.gnu.org/software/make/)
#### For C++
- [GCC](http://gcc.gnu.org/ "GCC, the GNU Compiler Collection")
- [flex](https://github.com/westes/flex "The Fast Lexical Analyzer")
- [Bison](https://www.gnu/software/bison/)
#### For OCaml
- [OCaml](https://ocaml.org/)
- [Menhir](http://gallium.inria.fr/~fpottier/menhir/)

| Action | Command |
| --- | --- |
| Download | `git clone https://github.com/hadleyel/ocppiler.git` |
| Build | `make` |
| Run | `./compiler.native file-name.txt` |
| Test | `make test` |
| Clean up | `make clean` |

## History
* **Final Project**
    * Added tuples to replace pairs!
    * Added `get` to replace `fst` and `snd`: it gets the nth item of a tuple!
* **Assignment 6**:
    * Added rudimentary state with refs, !s, and assignments
    * Added sequences
    * Added while loop
    * Known bugs: In corner cases, while loops become infinite. Oops.
* **Assignment 5**:
    * Added explicit types and typechecking
    * Added unit, pair, and pair pseudo-functions
    * Fixed messed up `fix` substitution
    * Cried less this time!
* **Assignment 4**:
    * Switched to OCaml, Lexing, and Menhir
    * Added let, function, variable, and function application support
    * Added substitution
    * Cried a lot
* **Assignment 3**:
    * Added tests for hand-rolled parser
    * Moved hand-rolled lexer/parser to its own folder
    * Implemented flex/Bison lexer/parser
* **Assignment 2**:
    * Deleted simple C++ I/O program
    * Created hand-rolled lexer/parser for a small, Lisp-like language
    * Booleans were temporarily C-like (i.e. just ints), but became `bool`s in a later release
* **Assignment 1**:
    * Created simple C++ I/O program
    * Added command-line flags
