#include <cctype> // for isspace, isdigit
#include <fstream> // for ifstream
#include <iostream> // for cout
#include <memory> // for unique_ptr
#include <vector> // for vector

using namespace std; // unless I use actual libraries

// Expression classes ----------------------------------------------------------
class Expression {
public:
  virtual int interpret() = 0;
};

class Literal: public Expression {
public:
  Literal (int data) {
    this->data = data;
  };
  int interpret() {
    return data;
  }
private:
  int data;
};

class Plus: public Expression {
public:
  Plus (Expression&, Expression&) {
    this.e1 = e1;
    this.e2 = e2;
  }
  int interpret() {
    int ret = e1.interpret() + e2.interpret();
  }
private:
  Expression *e1;
  Expression *e2;
};

// Token classes ---------------------------------------------------------------

class Token {
public:
  string kind; // bjarne was mean and didn't give me enum methods :(
  int data;
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
      Token add("LPAREN", -1);
      tokens.push_back(add);
    } else if (cur_char == ')') {
      Token add("RPAREN", -1);
      tokens.push_back(add);
    } else if (cur_char == '+') {
      Token add("PLUS", -1);
      tokens.push_back(add);
    } else if (isspace(cur_char)) {
      // do nothing
    } else if (isdigit(cur_char)) {
      string integer(1, cur_char);
      while (isdigit(input.peek())) {
        input.get(cur_char);
        integer.push_back(cur_char);
      }
      Token add("INT", stoi(integer));
      tokens.push_back(add);
    } else {
      cerr << "Unexpected char found: " << cur_char;
    }
  }
  return tokens;
}

// Parser ----------------------------------------------------------------------

class Parser {
public:
  Parser(vector<Token> tokens) {
    this->tokens = tokens;
    pos = 0;
  }
  unique_ptr<Expression> parse() {
    if (pos < (int) tokens.size()) {
      Token token = tokens.at(pos);
      string token_kind = token.kind;
      int token_data = token.data;
      if (token_kind.compare("INT")) {
        advance();
        Literal *ret = new Literal(token_data);
        return unique_ptr<Expression>(ret);
      } else if (token_kind.compare("LPAREN")) {
        consume("LPAREN");
        consume("PLUS");
        unique_ptr<Expression> e1 = parse();
        unique_ptr<Expression> e2 = parse();
        consume("RPAREN");
        Plus *ret = new Plus(e1, e2);
        return unique_ptr<Expression>(ret);
      } else {
        cerr << "Unexpected token" << token.to_string();
      }
    }
  }

private:
  int pos;
  vector<Token> tokens;
  void advance() {
    pos++;
  }
  void consume(string kind) {
    if (kind.compare(tokens.at(pos).kind)) {
      advance();
    } else {
      cerr << "Expected " << kind << ", found " << tokens.at(pos).kind;
    }
  }
};

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
  /* Token test
  for (unsigned int i = 0; i < tokens.size(); i++) {
    cout << tokens.at(i).to_string();
  } */
  // Parse it.
}
