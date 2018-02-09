#include <cctype> // for isspace, isdigit, isalpha, ispunct
#include <fstream> // for ifstream
#include <iostream> // for cout
#include <memory> // for shared_ptr
#include <vector> // for vector

using namespace std; // unless I use actual libraries

// Expression classes ----------------------------------------------------------
class Expression {
public:
  virtual int interpret() = 0;
};

class Literal: public Expression {
public:
  // normal constructor
  Literal(int data) {
    this->data = data;
  }
  // copy constructor
  Literal(const Literal &obj) {
    this->data = obj.data;
  }
  int interpret() {
    return data;
  }
private:
  int data;
};

class Plus: public Expression {
public:
  // normal constructor
  Plus (shared_ptr<Expression> left, shared_ptr<Expression> right) {
    this->left = left;
    this->right = right;
  }
  // copy constructor
  Plus (const Plus &obj) {
    this->left = obj.left;
    this->right = obj.right;
  }
  int interpret() {
    int ret = left->interpret() + right->interpret();
    return ret;
  }
private:
  shared_ptr<Expression> left;
  shared_ptr<Expression> right;
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
    } else if (cur_char == '-') {
      Token add("MINUS", -1);
      tokens.push_back(add);
    } else if (cur_char == '*') {
      Token add("TIMES", -1);
      tokens.push_back(add);
    } else if (cur_char == '/') {
      Token add("DIVIDE", -1);
      tokens.push_back(add);
    } else if (cur_char == 't') {
      string boole(1, cur_char);
      while (isalpha(input.peek())) {
        input.get(cur_char);
        boole.push_back(cur_char);
      }
      if (boole.compare("true") != 0) {
        cerr << "Unexpected token " << boole << "\n";
        exit(EXIT_FAILURE);
      }
      Token add("TRUE", -1);
      tokens.push_back(add);
    } else if (cur_char == 'f') {
      string boole(1, cur_char);
      while (isalpha(input.peek())) {
        input.get(cur_char);
        boole.push_back(cur_char);
      }
      if (boole.compare("false") != 0) {
        cerr << "Unexpected token " << boole << "\n";
        exit(EXIT_FAILURE);
      }
      Token add("FALSE", -1);
      tokens.push_back(add);
    } else if (cur_char == 'i') {
      string control(1, cur_char);
      while (isalpha(input.peek())) {
        input.get(cur_char);
        control.push_back(cur_char);
      }
      if (control.compare("if") != 0) {
        cerr << "Unexpected token " << control << "\n";
        exit(EXIT_FAILURE);
      }
      Token add("IF", -1);
      tokens.push_back(add);
    } else if (cur_char == '<') {
      string comparer(1, cur_char);
      while (ispunct(input.peek())) {
        input.get(cur_char);
        comparer.push_back(cur_char);
      }
      if (comparer.compare("<=") != 0) {
        cerr << "Unexpected token " << comparer << "\n";
        exit(EXIT_FAILURE);
      }
      Token add("<=", -1);
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
      exit(EXIT_FAILURE);
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
  shared_ptr<Expression> parse() {
    if (pos < (int) tokens.size()) {
      Token token = tokens.at(pos);
      string token_kind = token.kind;
      int token_data = token.data;
      if (token_kind.compare("INT") == 0) {
        advance();
        return make_shared<Literal>(token_data);
      } else if (token_kind.compare("LPAREN") == 0) {
        consume("LPAREN");
        consume("PLUS");
        shared_ptr<Expression> left = parse();
        shared_ptr<Expression> right = parse();
        consume("RPAREN");
        return make_shared<Plus>(left, right);
      } else {
        cerr << "Unexpected token" << token.to_string() << "\n";
      }
    }
    cerr << "Parsing error\n";
    exit(EXIT_FAILURE);
  }

private:
  int pos;
  vector<Token> tokens;
  void advance() {
    pos++;
  }
  void consume(string kind) {
    if (kind.compare(tokens.at(pos).kind) == 0) {
      advance();
    } else {
      cerr << "Expected " << kind << ", found " << tokens.at(pos).kind << "\n";
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
  //Token test
  for (unsigned int i = 0; i < tokens.size(); i++) {
    cout << tokens.at(i).to_string() << " ";
  }
  // Parse it.
  //Parser parser = Parser(tokens);
  //shared_ptr<Expression> e = parser.parse();
  //cout << e->interpret() << "\n";
}
