#include <fstream> // for ifstream
#include <vector> // for vector

class Token {
public:
  std::string kind; // bjarne was mean and didn't give me enum methods :(
  int data;
  Token(std::string kind, int data);
  Token(const Token &obj);
  void operator= (const Token &obj);
  std::string to_string();
};

std::vector<Token> lex(std::ifstream& input);
