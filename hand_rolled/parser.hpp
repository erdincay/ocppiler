#ifndef PARSER_HPP
#define PARSER_HPP

#include <memory> // for shared_ptr
#include <vector> // for vector

#include "lexer.hpp"

typedef enum { INT, BOOL } return_kind_t;

typedef struct {
  return_kind_t type;
  int integer;
  bool boolean;
} return_t;

class Expression {
public:
  virtual return_t interpret() = 0;
  virtual std::string toString() = 0;
};

void evaluate(std::shared_ptr<Expression> e);

class Literal: public Expression {
public:
  Literal(Token token);
  Literal(const Literal &obj);
  return_t interpret();
  std::string toString();

private:
  return_t data;
};

class Operator: public Expression {
public:
  Operator (std::string name, std::shared_ptr<Expression> left, std::shared_ptr<Expression> right);
  Operator (const Operator &obj);
  return_t interpret();
  std::string toString();
private:
  std::string name;
  std::shared_ptr<Expression> left;
  std::shared_ptr<Expression> right;
};

class Conditional: public Expression {
public:
  Conditional (std::string name, std::shared_ptr<Expression> condition, std::shared_ptr<Expression> then, std::shared_ptr<Expression> els);
  Conditional (const Conditional &obj);
  return_t interpret();
  std::string toString();
private:
  std::string name;
  std::shared_ptr<Expression> condition;
  std::shared_ptr<Expression> then;
  std::shared_ptr<Expression> els;
};

class Parser {
public:
  Parser(std::vector<Token> tokens);
  Parser (const Parser &obj);
  std::shared_ptr<Expression> parse();

private:
  int pos;
  std::vector<Token> tokens;
  void advance();
  void consume(std::string kind);
};

#endif
