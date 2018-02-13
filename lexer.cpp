#include <cctype> // for isspace, isdigit, isalpha, ispunct
#include <fstream> // for ifstream
#include <iostream> // for cerr
#include <vector> // for vector

#include "lexer.hpp"

//Token
Token::Token(std::string kind, int data) {
  this->kind = kind;
  this->data = data;
}
Token::Token(const Token &obj) {
  this->kind = obj.kind;
  this->data = obj.data;
}
void Token::operator= (const Token &obj) {
  this->kind = obj.kind;
  this->data = obj.data;
}
std::string Token::to_string() {
  if (data == -1) {
    return kind;
  } else {
    std::string number = std::to_string(data);
    std::string ret;
    ret.append(kind);
    ret.append("(");
    ret.append(number);
    ret.append(")");
    return ret;
  }
}

std::vector<Token> lex(std::ifstream& input) {
  char cur_char;
  std::vector<Token> tokens;
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
      std::string boole(1, cur_char);
      while (isalpha(input.peek())) {
        input.get(cur_char);
        boole.push_back(cur_char);
      }
      if (boole.compare("true") != 0) {
        std::cerr << "Unexpected token " << boole << "\n";
        exit(EXIT_FAILURE);
      }
      Token add("TRUE", 1);
      tokens.push_back(add);
    } else if (cur_char == 'f') {
      std::string boole(1, cur_char);
      while (isalpha(input.peek())) {
        input.get(cur_char);
        boole.push_back(cur_char);
      }
      if (boole.compare("false") != 0) {
        std::cerr << "Unexpected token " << boole << "\n";
        exit(EXIT_FAILURE);
      }
      Token add("FALSE", 0);
      tokens.push_back(add);
    } else if (cur_char == 'i') {
      std::string control(1, cur_char);
      while (isalpha(input.peek())) {
        input.get(cur_char);
        control.push_back(cur_char);
      }
      if (control.compare("if") != 0) {
        std::cerr << "Unexpected token " << control << "\n";
        exit(EXIT_FAILURE);
      }
      Token add("IF", -1);
      tokens.push_back(add);
    } else if (cur_char == '<') {
      std::string comparer(1, cur_char);
      while (ispunct(input.peek())) {
        input.get(cur_char);
        comparer.push_back(cur_char);
      }
      if (comparer.compare("<=") != 0) {
        std::cerr << "Unexpected token " << comparer << "\n";
        exit(EXIT_FAILURE);
      }
      Token add("LEQ", -1);
      tokens.push_back(add);
    } else if (isspace(cur_char)) {
      // do nothing
    } else if (isdigit(cur_char)) {
      std::string integer(1, cur_char);
      while (isdigit(input.peek())) {
        input.get(cur_char);
        integer.push_back(cur_char);
      }
      Token add("INT", stoi(integer));
      tokens.push_back(add);
    } else {
      std::cerr << "Unexpected char found: " << cur_char;
      exit(EXIT_FAILURE);
    }
  }
  return tokens;
}
