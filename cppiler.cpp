#include <cctype> // for isspace, isdigit, isalpha, ispunct
#include <fstream> // for ifstream
#include <iostream> // for cout
#include <memory> // for shared_ptr
#include <vector> // for vector

using namespace std; // unless I use actual libraries

// PARSER: Expression classes --------------------------------------------------
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

class Operator: public Expression {
public:
  // normal constructor
  Operator (string name, shared_ptr<Expression> left, shared_ptr<Expression> right) {
    this->name = name;
    this->left = left;
    this->right = right;
  }
  // copy constructor
  Operator (const Operator &obj) {
    this->name = obj.name;
    this->left = obj.left;
    this->right = obj.right;
  }
  int interpret() {
    int ret;
    if (!name.compare("PLUS")) {
      ret = left->interpret() + right->interpret();
    } else if (!name.compare("MINUS")) {
      ret = left->interpret() - right->interpret();
    } else if (!name.compare("TIMES")) {
      ret = left->interpret() * right->interpret();
    } else if (!name.compare("DIVIDE")) {
      int leftside = left->interpret();
      int rightside = right->interpret();
      if (rightside == 0) {
        perror("Division by zero is not permitted.\n");
        exit(EXIT_FAILURE);
      }
      ret = leftside / rightside;
    } else if (!name.compare("LEQ")) {
      ret = left->interpret() <= right->interpret(); // TODO: real booleans
    } else {
      cerr << "Unexpected operator name " << name << "\n";
      exit(EXIT_FAILURE);
    }
    return ret;
  }
private:
  string name;
  shared_ptr<Expression> left;
  shared_ptr<Expression> right;
};

class Conditional: public Expression {
public:
  // normal constructor
  Conditional (string name, shared_ptr<Expression> condition, shared_ptr<Expression> then, shared_ptr<Expression> els) {
    this->name = name;
    this->condition = condition;
    this->then = then;
    this->els = els;
  }
  // copy constructor
  Conditional (const Conditional &obj) {
    this->name = obj.name;
    this->condition = obj.condition;
    this->then = obj.then;
    this->els = obj.els;
  }
  int interpret() {
    if(condition->interpret()) {
      return then->interpret();
    } else {
      return els->interpret();
    }
  }
private:
  string name;
  shared_ptr<Expression> condition;
  shared_ptr<Expression> then;
  shared_ptr<Expression> els;
};

// LEXER: Token classes --------------------------------------------------------
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
      Token add("TRUE", 1);
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
      Token add("FALSE", 0);
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
      Token add("LEQ", -1);
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
      if (!token_kind.compare("INT")) {
        advance();
        return make_shared<Literal>(token_data);
      } else if (!token_kind.compare("TRUE") || (!token_kind.compare("FALSE"))) {
        advance();
        return make_shared<Literal>(token_data);
      } else if (!token_kind.compare("LPAREN")) {
        consume("LPAREN");
        token = tokens.at(pos);
        token_kind = token.kind;
        if (!token_kind.compare("PLUS") || !token_kind.compare("MINUS")
            || !token_kind.compare("TIMES") || !token_kind.compare("DIVIDE")
            || !token_kind.compare("LEQ")) {
          consume(token_kind);
          shared_ptr<Expression> left = parse();
          shared_ptr<Expression> right = parse();
          consume("RPAREN");
          return make_shared<Operator>(token_kind, left, right);
        } else if (!token_kind.compare("IF")) {
          consume("IF");
          shared_ptr<Expression> condition = parse();
          shared_ptr<Expression> then = parse();
          shared_ptr<Expression> els = parse();
          consume("RPAREN");
          return make_shared<Conditional>("IF", condition, then, els);
        } else {
          cerr << "Unexpected token" << token.to_string() << "\n";
        }
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
    if (!kind.compare(tokens.at(pos).kind)) {
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
  /* for (unsigned int i = 0; i < tokens.size(); i++) {
    cout << tokens.at(i).to_string() << " ";
  }
  cout << "\n"; */
  // Parse it.
  Parser parser = Parser(tokens);
  shared_ptr<Expression> e = parser.parse();
  cout << e->interpret() << "\n";
}
