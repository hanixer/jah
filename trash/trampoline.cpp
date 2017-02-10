#include <iostream>
#include <memory>
#include <string>
#include <map>

using namespace std;

class Val
{
public:
   virtual ~Val() {}
};

typedef std::shared_ptr<Val> ValPtr;
class Bounce;

typedef std::map<std::string, ValPtr> Env;

class Cont
{
public:
   virtual ~Cont() {}
   virtual Bounce apply(ValPtr val) = 0;
};

typedef std::shared_ptr<Cont> ContPtr;

class Exp
{
public:
   virtual ~Exp() {}
   virtual Bounce eval(const Env& env, ContPtr cont) = 0;
};
typedef std::shared_ptr<Exp> ExpPtr;

class Bounce
{
public:
   Bounce(ValPtr val) : mVal(val) {}
   Bounce(ContPtr cont) : mCont(cont) {}

   bool hasCont() { return mCont.operator bool(); }
   Bounce applyCont() { return mCont->apply(); }

private:
   ValPtr mVal;
   ContPtr mCont;
};

class NumVal : public Val
{
public:
   NumVal(int n) : num(n) {}
   int num;
};

class NumExp : public Exp
{
public:
   NumExp(int n) : mNum(n) {}

   Bounce eval(const Env &, ContPtr ) override
   {
      return ValPtr(new NumVal{mNum});
   }

private:
   int mNum;
};

class VarExp : public Exp
{
public:
   VarExp(const std::string& v) : mVar(v) {}

   Bounce eval(const Env &env, ContPtr ) override
   {
      if (env.count(mVar) > 0)
      {
         ValPtr val = env.at(mVar);
         return Bounce(val);
      }
      else
      {
         throw std::exception("Variable was not found");
      }
   }
private:
   std::string mVar;
};

class ProcVal : public Val
{
public:
   ProcVal(const std::string& s, ExpPtr b, Env e) : arg(s), body(b), env(e) {}
   std::string arg;
   ExpPtr body;
   Env env;
};

class ProcExp : public Exp
{
public:
   ProcExp(const std::string& s, ExpPtr b) : arg(s), body(b) {}

   Bounce eval(const Env &env, ContPtr ) override
   {
      return Bounce(ValPtr(new ProcVal{arg, body, env}));
   }
private:
   std::string arg;
   ExpPtr body;
};

class CallExp : public Exp
{
public:
   CallExp(ExpPtr e1, ExpPtr e2) : mOperator(e1), mOperand(e2) {}

   Bounce eval(const Env &env, ContPtr cont) override
   {
      class MyCont : public Cont {
         MyCont(const Env& env, ContPtr finalCont) : mEnv(env), mFinalCont(finalCont) {}

         Bounce apply() override
         {

         }
      private:
         Env mEnv;
         ContPtr mFinalCont;
      };

      return mOperator->eval(env, )
   }

private:
   ExpPtr mOperator;
   ExpPtr mOperand;
};

void trampoline(ExpPtr exp)
{
   Env env;
   class EndCont : public Cont { Bounce apply() override { std::cout << "End."; return Bounce(ValPtr()); } };
   ContPtr endCont(new EndCont);
   Bounce bounce = exp->eval(env, endCont);

   while (bounce.hasCont())
   {
      bounce = bounce.applyCont();
   }
}

int main()
{
   cout << "Hello World!" << endl;
   ValPtr vvv(new ProcVal{"", 0, Env()});

   return 0;
}

