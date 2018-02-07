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

class Token {
public:
  string kind; // bjarne was mean and didn't give me enum methods :(
  int data;
  Token(string kind) {
    Token(kind, -1);
  }
  Token(string kind, int data) {
    this->kind = kind;
    this->data = data;
  }

  string to_string() {
    if (data == -1) {
      return kind;
    } else {
      string number = std::to_string(data);
      string ret;
      ret.append(kind);
      ret.append("(");
      ret.append(number);
      ret.append(")");
      return ret;
    }
  }
};

// Lexer -----------------------------------------------------------------------

vector<Token> lex(ifstream& input) {
  char cur_char;
  vector<Token> tokens;
  while (!input.eof()) {
    input.get(cur_char);
    if (cur_char == '(') {
      Token add("LPAREN");
      tokens.push_back(add);
    } else if (cur_char == ')') {
      Token add("RPAREN");
      tokens.push_back(add);
    } else if (cur_char == '+') {
      Token add("RPAREN");
      tokens.push_back(add);
    } else if (isspace(cur_char)) {
      // do nothing
    } else if (isdigit(cur_char)) {
      string integer = "" + cur_char;
      input.get(cur_char);
      while (isdigit(cur_char)) {
        integer += cur_char;
        input.get(cur_char);
      }
      cout << "integer was " << integer;
      Token add("INT", stoi(integer));
      tokens.push_back(add);
    } else {
      cerr << "Unexpected char found: " << cur_char;
    }
  }
  return tokens;
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
  vector<Token> tokens = lex(input);
  for (unsigned int i = 0; i<10;i++) {
    tokens.at(i).to_string();
  }

  // Parse it.
}

// Notes -----------------------------------------------------------------------
//declare interface Exp
// with a subclass literal (integer) implements exp, which returns value
// with a subclass plus implements exp that can have two exp children,
// both have an intepretation function
// recursively call interpret on sub functions
