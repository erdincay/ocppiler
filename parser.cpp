#include <iostream> // for cerr
#include <memory> // for shared_ptr
#include <string> // for to_string
#include <vector> // for vector

#include "lexer.hpp"
#include "parser.hpp"

void evaluate(std::shared_ptr<Expression> e) {
  return_t ret = e->interpret();
  return_kind_t type = ret.type;
  switch (type) {
  case INT:
    std::cout << ret.integer << "\n";
    break;
  case BOOL:
    if (ret.boolean == true) {
      std::cout << "true" << "\n";
    } else if (ret.boolean == false) {
      std::cout << "false" << "\n";
    } else {
      std::cout << "improper boolean\n";
    }
    break;
  default:
    std::cerr << "Unrecognized type\n";
    exit(EXIT_FAILURE);
  }
}

// Literal
Literal::Literal(Token token) {
  if (!token.kind.compare("TRUE")) {
    data.type = BOOL;
    data.boolean = true;
  } else if (!token.kind.compare("FALSE")) {
    data.type = BOOL;
    data.boolean = false;
  } else if (!token.kind.compare("INT")) {
    data.type = INT;
    data.integer = token.data;
  } else {
    std::cerr << "Invalid token.\n";
    exit(EXIT_FAILURE);
  }
}
Literal::Literal(const Literal &obj) {
  this->data = obj.data;
}
return_t Literal::interpret() {
  return data;
}
std::string Literal::toString() {
  if ((data.type == BOOL) && (data.boolean == true)) {
    return "true";
  } else if ((data.type == BOOL) && (data.boolean == false)) {
    return "false";
  } else {
    return std::to_string(data.integer);
  }
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
return_t Operator::interpret() {
  return_t ret;
  return_t left_side = left->interpret();
  return_t right_side = right->interpret();

  if ((left_side.type == INT) && (right_side.type == INT)) {
    if (!name.compare("PLUS")) {
      ret.type = INT;
      ret.integer = left_side.integer + right_side.integer;
    } else if (!name.compare("MINUS")) {
      ret.type = INT;
      ret.integer = left_side.integer - right_side.integer;
    } else if (!name.compare("TIMES")) {
      ret.type = INT;
      ret.integer = left_side.integer * right_side.integer;
    } else if (!name.compare("DIVIDE")) {
      if (right_side.integer == 0) {
        std::cerr << "Division by zero is not permitted";
        exit(EXIT_FAILURE);
      }
      ret.type = INT;
      ret.integer = left_side.integer / right_side.integer;
    } else if (!name.compare("LEQ")) {
      ret.type = BOOL;
      ret.boolean = left_side.integer <= right_side.integer;
    } else {
      std::cerr << "Unexpected operator name " << name << "\n";
      exit(EXIT_FAILURE);
    }
    return ret;
  } else {
    std::cerr << "Operation was not made between ints.\n";
    exit(EXIT_FAILURE);
  }
}

std::string Operator::toString() {
  std::string ret;
  ret.append("(");
  if (!name.compare("PLUS")) {
    ret.append("+");
  } else if (!name.compare("MINUS")) {
    ret.append("-");
  } else if (!name.compare("TIMES")) {
    ret.append("*");
  } else if (!name.compare("DIVIDE")) {
    ret.append("/");
  } else if (!name.compare("LEQ")) {
    ret.append("<=");
  } else {
    std::cerr << "Error determining operator";
    exit(EXIT_FAILURE);
  }
  ret.append(" ");
  ret.append(left->toString());
  ret.append(" ");
  ret.append(right->toString());
  ret.append(")");
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
return_t Conditional::interpret() {
  return_t cond = condition->interpret();
  return_t thn = then->interpret();
  return_t el = els->interpret();

  if (cond.type == BOOL) {
    if(cond.boolean) {
      return thn;
    } else {
      return el;
    }
  } else {
    std::cerr << "Conditional was not boolean!\n";
    exit(EXIT_FAILURE);
  }
}

std::string Conditional::toString() {
  std::string ret;
  ret.append("(if ");
  ret.append(condition->toString());
  ret.append(" ");
  ret.append(then->toString());
  ret.append(" ");
  ret.append(els->toString());
  ret.append(")");
  return ret;
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
    if (!token_kind.compare("INT")) {
      advance();
      return std::make_shared<Literal>(token);
    } else if (!token_kind.compare("TRUE") || (!token_kind.compare("FALSE"))) {
      advance();
      return std::make_shared<Literal>(token);
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
