#include <iostream> // for cerr
#include <memory> // for shared_ptr
#include <vector> // for vector

#include "lexer.hpp"
#include "parser.hpp"

// Literal
Literal::Literal(int data) {
  this->data = data;
}
Literal::Literal(const Literal &obj) {
  this->data = obj.data;
}
int Literal::interpret() {
  return data;
}
// Operator
Operator::Operator (std::string name, std::shared_ptr<Expression> left, std::shared_ptr<Expression> right) {
  this->name = name;
  this->left = left;
  this->right = right;
}
Operator::Operator (const Operator &obj) {
  this->name = obj.name;
  this->left = obj.left;
  this->right = obj.right;
}
int Operator::interpret() {
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
    std::cerr << "Unexpected operator name " << name << "\n";
    exit(EXIT_FAILURE);
  }
  return ret;
}

// Conditional
Conditional::Conditional (std::string name, std::shared_ptr<Expression> condition, std::shared_ptr<Expression> then, std::shared_ptr<Expression> els) {
  this->name = name;
  this->condition = condition;
  this->then = then;
  this->els = els;
}
Conditional::Conditional (const Conditional &obj) {
  this->name = obj.name;
  this->condition = obj.condition;
  this->then = obj.then;
  this->els = obj.els;
}
int Conditional::interpret() {
  if(condition->interpret()) {
    return then->interpret();
  } else {
    return els->interpret();
  }
}

// Parser
Parser::Parser(std::vector<Token> tokens) {
  this->tokens = tokens;
  pos = 0;
}
Parser::Parser (const Parser &obj) {
  this->tokens = obj.tokens;
  this->pos = obj.pos;
}
std::shared_ptr<Expression> Parser::parse() {
  if (pos < (int) tokens.size()) {
    Token token = tokens.at(pos);
    std::string token_kind = token.kind;
    int token_data = token.data;
    if (!token_kind.compare("INT")) {
      advance();
      return std::make_shared<Literal>(token_data);
    } else if (!token_kind.compare("TRUE") || (!token_kind.compare("FALSE"))) {
      advance();
      return std::make_shared<Literal>(token_data);
    } else if (!token_kind.compare("LPAREN")) {
      consume("LPAREN");
      token = tokens.at(pos);
      token_kind = token.kind;
      if (!token_kind.compare("PLUS") || !token_kind.compare("MINUS")
          || !token_kind.compare("TIMES") || !token_kind.compare("DIVIDE")
          || !token_kind.compare("LEQ")) {
        consume(token_kind);
        std::shared_ptr<Expression> left = parse();
        std::shared_ptr<Expression> right = parse();
        consume("RPAREN");
        return std::make_shared<Operator>(token_kind, left, right);
      } else if (!token_kind.compare("IF")) {
        consume("IF");
        std::shared_ptr<Expression> condition = parse();
        std::shared_ptr<Expression> then = parse();
        std::shared_ptr<Expression> els = parse();
        consume("RPAREN");
        return std::make_shared<Conditional>("IF", condition, then, els);
      } else {
        std::cerr << "Unexpected token" << token.to_string() << "\n";
      }
    }
  }
  std::cerr << "Parsing error\n";
  exit(EXIT_FAILURE);
}
void Parser::advance() {
  pos++;
}
void Parser::consume(std::string kind) {
  if (!kind.compare(tokens.at(pos).kind)) {
    advance();
  } else {
    std::cerr << "Expected " << kind << ", found " << tokens.at(pos).kind << "\n";
  }
}
