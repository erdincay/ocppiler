#include <fstream> // for ifstream
#include <getopt.h> // for getopt
#include <iostream> // for cerr, cout
#include <memory> // for shared_ptr
#include <vector> // for vector

#include "lexer.hpp"
#include "parser.hpp"

static int lex_flag;
static int parse_flag;

int main(int argc, char *argv[]) {

  // Open file.
  std::ifstream input;
  input.open(argv[1]);
  if (!input) {
    std::cerr << "Unable to open file.\n";
    exit(EXIT_FAILURE);
  }

  const struct option flags[] = {
    {"lex",   no_argument, &lex_flag,   1},
    {"parse", no_argument, &parse_flag, 1},
    {0,       0,           0,           0}
  };

  while (getopt_long_only(argc, argv, "", flags, NULL) != -1);

  if (lex_flag && !parse_flag) {
    std::vector<Token> tokens = lex(input);
    //Token test
    for (unsigned int i = 0; i < tokens.size(); i++) {
      std::cout << tokens.at(i).to_string() << " ";
    }
    std::cout << "\n";
  } else if (!lex_flag && parse_flag) {
    std::vector<Token> tokens = lex(input);
    Parser parser (tokens);
    std::shared_ptr<Expression> e = parser.parse();
    std::cout << e->toString() << "\n";
  } else {
    std::vector<Token> tokens = lex(input);
    Parser parser (tokens);
    std::shared_ptr<Expression> e = parser.parse();
    std::cout << e->interpret() << "\n";
  }
}
