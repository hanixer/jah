#include <iostream>
#include <string>
#include <cstdio>
#include <cstdlib>
#include <memory>

enum Token {
  TOK_EOF = -1,

  // commands
  TOK_DEF = -2,
  TOK_EXTERN = -3,

  // primary
  TOK_IDENTIFIER = -4,
  TOK_NUMBER = -5,
};

static std::string stringVal;
static double doubleVal;

int getTok()
{
   static char lastChar = ' ';

   while (isspace(lastChar))
      lastChar = getchar();

   if (lastChar == EOF)
      return TOK_EOF;
   else if (isalpha(lastChar))
   {
      stringVal = lastChar;
      while (isalnum(lastChar = getchar()))
      {
         stringVal += lastChar;
      }

      if (stringVal == "def")
         return TOK_DEF;
      else if (stringVal == "extern")
         return TOK_EXTERN;
      else
         return TOK_IDENTIFIER;
   }
   else if (isdigit(lastChar) || lastChar == '.')
   {
      do
      {
         stringVal += lastChar;
         lastChar = getchar();
      } while (isdigit(lastChar) || lastChar == '.');
      doubleVal = strtod(stringVal.c_str(), NULL);

      return TOK_NUMBER;
   }
   else if (lastChar == '#')
   {
      do
      {
         lastChar = getchar();
      } while (lastChar != EOF || lastChar == '\r' || lastChar == '\n');

      return getTok();
   }

   if (lastChar == EOF)
      return TOK_EOF;

   int ch = lastChar;
   lastChar = getchar();
   return ch;
}

class ExprAst
{
public:
   virtual ~ExprAst() {}
};

typedef std::unique_ptr<ExprAst> ExprAstPtr;

class NumExpr : public ExprAst
{
public:
   NumExpr(double v) : value(v) {}
   double value;
};

class VariableExprAst : public ExprAst
{
public:
    VariableExprAst(std::string s) : name(s) {}
    std::string name;
};

class BinaryExprAst : public ExprAst
{
public:
    BinaryExprAst(char op, ExprAstPtr lhs, ExprAstPtr rhs)
        : op(op), lhs(std::move(lhs)), rhs(std::move(rhs))
    {}

    char op;
    ExprAstPtr lhs;
    ExprAstPtr rhs;
};

ExprAstPtr parseNumExpr()
{
   if (getTok() == TOK_NUMBER)
      return std::make_unique<NumExpr>(doubleVal);
   else
      return nullptr;
}


int main()
{
   int tok;
   do
   {
      tok = getTok();
      std::cout << tok  << '\t' << stringVal << '\t' << doubleVal << '\n';

   } while(tok != TOK_EOF);

   return 0;
}
