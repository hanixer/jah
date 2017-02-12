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
    virtual Bounce apply() = 0;
    ValPtr val;
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

    Bounce eval(const Env &, ContPtr cont) override
    {
        cont->val = ValPtr(new NumVal{ mNum });
        return Bounce(cont);
    }

private:
    int mNum;
};

class VarExp : public Exp
{
public:
    VarExp(const std::string& v) : mVar(v) {}

    Bounce eval(const Env &env, ContPtr cont) override
    {
        if (env.count(mVar) > 0)
        {
            cont->val = env.at(mVar);

            return Bounce(cont);
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

    Bounce eval(const Env &env, ContPtr cont) override
    {
        cont->val = ValPtr(new ProcVal{ arg, body, env });
        return Bounce(cont);
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
        class OperandCont : public Cont
        {
        public:
            OperandCont(ContPtr finalCont, ValPtr procVal) : mFinalCont(finalCont), mProcVal(procVal) {}

            Bounce apply() override
            {
                if (ProcVal* procVal = dynamic_cast<ProcVal*>(mProcVal.get()))
                {
                    procVal->body;
                    procVal->arg;
                    Env env = procVal->env;
                    env[procVal->arg] = val;
                    return procVal->body->eval(env, mFinalCont);
                }
                else
                {
                    throw std::exception("Proc value is expected.");
                }
            }
        private:
            ContPtr mFinalCont;
            ValPtr mProcVal;
        };

        class OperatorCont : public Cont {
        public:
            OperatorCont(const Env& env, ContPtr finalCont, ExpPtr operand) :
                mEnv(env),
                mFinalCont(finalCont),
                mOperand(operand)
            {}

            Bounce apply() override
            {
                return mOperand->eval(mEnv, ContPtr(new OperandCont(mFinalCont, val)));
            }
        private:
            Env mEnv;
            ContPtr mFinalCont;
            ExpPtr mOperand;
        };

        return mOperator->eval(env, ContPtr(new OperatorCont(env, cont, mOperand)));
    }

private:
    ExpPtr mOperator;
    ExpPtr mOperand;
};

void trampoline(ExpPtr exp)
{
    Env env;
    class EndCont : public Cont
    {
        Bounce apply() override
        {
            if (NumVal* numv = dynamic_cast<NumVal*>(val.get()))
            {
                std::cout << "int is found: " << numv->num << "\n";
            }
            std::cout << "End."; return Bounce(ValPtr());
        }
    };
    ContPtr endCont(new EndCont);
    Bounce bounce = exp->eval(env, endCont);

    while (bounce.hasCont())
    {
        bounce = bounce.applyCont();
    }
}

ExpPtr ne(int n) { return ExpPtr(new NumExp(n)); }
ExpPtr ve(std::string s) { return ExpPtr(new VarExp(s)); }
ExpPtr pe(std::string s, ExpPtr b) { return ExpPtr(new ProcExp(s, b)); }
ExpPtr ce(ExpPtr e1, ExpPtr e2) { return ExpPtr(new CallExp(e1, e2)); }

int main()
{
    cout << "Hello World!" << endl;

    // (((lambda (x) lambda (y) x) 5) 33) => 5
    trampoline(ce(ce(pe("x",pe("y",ve("x"))),ne(5)),ne(33)));
    return 0;
}

