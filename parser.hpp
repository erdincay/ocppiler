#include <memory> // for shared_ptr
#include <vector> // for vector

class Expression {
public:
  virtual int interpret();
};

class Literal: public Expression {
public:
  Literal(int data);
  Literal(const Literal &obj);
  int interpret();

private:
  int data;
};

class Operator: public Expression {
public:
  Operator (std::string name, std::shared_ptr<Expression> left, std::shared_ptr<Expression> right);
  Operator (const Operator &obj);
  int interpret();
private:
  std::string name;
  std::shared_ptr<Expression> left;
  std::shared_ptr<Expression> right;
};

class Conditional: public Expression {
public:
  Conditional (std::string name, std::shared_ptr<Expression> condition, std::shared_ptr<Expression> then, std::shared_ptr<Expression> els);
  Conditional (const Conditional &obj);
  int interpret();
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
