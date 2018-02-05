#include <fstream> // for ifstream
#include <iostream> // for cout
#include <string> // for .compare
#include <vector> // for vector

using namespace std; // unless I use actual libraries

// Expression definitions
class Exp {
  virtual void interpret();
};

class Integer: public Exp {
public:
  void interpret();
};

class Plus: public Exp {
public:
  void interpret();
private:
  Exp left;
  Exp right;
};

enum Token { lparen, rparen, plus, integer };

vector<Token> * lex(ifstream& input) {
  char cur_char;
  while (!input.eof()) {
    input.get(cur_char);
    cout << cur_char;
  }
  return NULL;
}

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

//declare interface Exp
// with a subclass literal (integer) implements exp, which returns value
// with a subclass plus implements exp that can have two exp children,
// both have an intepretation function
// recursively call interpret on sub functions
