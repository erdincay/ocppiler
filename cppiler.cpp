#include <cctype> // for isspace, isdigit
#include <fstream> // for ifstream
#include <iostream> // for cout
#include <vector> // for vector

using namespace std; // unless I use actual libraries

// Expression classes ----------------------------------------------------------
class Expression {
public:
  virtual int interpret() = 0;
};

class Literal: public Expression {
public:
  Literal (int);
  int interpret();
private:
  int data;
};

class Plus: public Expression {
public:
  Plus (Expression&, Expression&);
  int interpret();
private:
  Expression *left;
  Expression *right;
};

// Token classes ---------------------------------------------------------------
enum TokenKind {
  INT, PLUS, LPAREN, RPAREN;
};

class Token {
public
};

// Lexer -----------------------------------------------------------------------

vector<Token> lex(ifstream& input) {
  char cur_char;
  vector<Token> ret;
  while (!input.eof()) {
    input.get(cur_char);
    if (cur_char == '(') {
      cout << "lparen ";
    } else if (cur_char == ')') {
      cout << "rparen ";
    } else if (cur_char == '+') {
      cout << "plus ";
    } else if (isspace(cur_char)) {
      cout << "space ";
    } else if (isdigit(cur_char)) {
      cout << "digit ";
    } else {
      cout << "\nYOU DONE FUCKED UP\n";
    }
  }
  return ret;
}

// Parser ----------------------------------------------------------------------

// Driver ----------------------------------------------------------------------

int main(int argc, char *argv[]) {
  // Open file.
  ifstream input;
  input.open(argv[1]);
  if (!input) {
    cout << "Unable to open file.\n";
    exit(EXIT_FAILURE);
  }

  // Lex it.
  lex(input);

  // Parse it.
}

// Notes -----------------------------------------------------------------------
//declare interface Exp
// with a subclass literal (integer) implements exp, which returns value
// with a subclass plus implements exp that can have two exp children,
// both have an intepretation function
// recursively call interpret on sub functions
