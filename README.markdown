# cppiler
A bare-bones compiler written in C++, flex, and Bison, as well as OCaml, Lexing, and Menhir; made by [@hadleyel](https://github.com/hadleyel "Hadley") to experiment with programming language implementation.

## Grammar
This will be frequently modified and added to.
```
e ::= n | (e) | e1 + e2 | e1 - e2 | e1 * e2 | e1 / e2 | true | false | e1 <= e2 | if e1 then e2 else e3 | x | let x = e1 in e2 | fun x -> e | e1 e2
```
## Compiler
### Dependencies
- [Git](https://git-scm.com/)
- [Make](https://www.gnu.org/software/make/)
- [GCC](http://gcc.gnu.org/ "GCC, the GNU Compiler Collection")
- [flex](https://github.com/westes/flex "The Fast Lexical Analyzer")
- [Bison](https://www.gnu/software/bison/)

| Action | Command |
| --- | --- |
| Download | `git clone https://github.com/hadleyel/ocppiler.git` |
| Build | `make` |
| Run | `./compiler.native file-name.txt` |
| Test | `make test` |
| Clean up | `make clean` |

## History
* **Assignment 4**:
    * Switched to OCaml, Lexing, and Menhir
    * Added let, function, variable, and function application support
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
