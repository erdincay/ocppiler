#include <fstream> // for ifstream
#include <iostream> // for cerr, cout
#include <memory> // for shared_ptr
#include <vector> // for vector

#include "lexer.hpp"
#include "parser.hpp"

int main(int argc, char *argv[]) {
  // Open file.
  std::ifstream input;
  input.open(argv[1]);
  if (!input) {
    std::cerr << "Unable to open file.\n";
    exit(EXIT_FAILURE);
  }

  // Lex it.
  std::vector<Token> tokens = lex(input);
  //Token test
  /* for (unsigned int i = 0; i < tokens.size(); i++) {
    cout << tokens.at(i).to_string() << " ";
  }
  cout << "\n"; */
  // Parse it.
  Parser parser (tokens);
  std::shared_ptr<Expression> e = parser.parse();
  std::cout << e->interpret() << "\n";
}
