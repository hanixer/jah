#include <iostream>
#include <functional>
#include <string>
#include <map>
#include <list>
#include <cassert>

using std::cout;

template <typename D, typename B>
class IsDerivedFrom
{
	class Yes { char a[1]; };
	class No { char a[2]; };

	static Yes test(B*);
	static No test(...);
public:
	enum { Res = sizeof(test(static_cast<D*>(0))) == sizeof(Yes) };
};

template <bool b>
struct Bool {};

template <typename H>
struct TopState
{
	typedef H Host;
	typedef void Base;

	static void init(Host& h);
	static void enter(Host& h);
	static void exit(Host& h);
	virtual void handle(Host& h);
};

template <typename H, int id, typename B>
struct CompositeState;

template <typename H, int id, typename B = TopState<H> >
struct CompositeState : B
{
	typedef H Host;
	typedef B Base;

	static void init(Host& h);
	static void enter(Host& h);
	static void exit(Host& h);
	virtual void handle(Host& h);
};

template <typename H, int id, typename B>
struct LeafState : B
{
	typedef H Host;
	typedef B Base;

	static void init(Host& h) {} // It is a leaf state, so no child substates are present.
	static void enter(Host& h);
	static void exit(Host& h);
	virtual void handle(Host& h);

	static const LeafState<H, id, B> obj;
};

template <typename H, int id, typename B>
const LeafState<H, id, B> LeafState<H, id, B>::obj;

template <typename H, typename C, typename S, typename D>
struct Transition
{
	typedef H Host;
	typedef C Current;
	typedef S Source;
	typedef D Destination;

	static void exitActions(Host& h, Bool<false>)
	{
		Current::exit(h);
		Transition<Host, Current::Base, Source, Destination>::exitActions(h, Bool<IsDerivedFrom<Destination, Current::Base>::Res>());
	}
	static void exitActions(Host& h, Bool<true>)
	{
	}

	static void enterActions(Host& h, Bool<false>)
	{
		Transition<Host, Current::Base, Source, Destination>::enterActions(h, Bool<IsDerivedFrom<Source, Current::Base>::Res>());
		Current::enter(h);
	}
	static void enterActions(Host& h, Bool<true>)
	{
	}

	Transition(Host& h)
		: mHost(h)
	{
		exitActions(mHost, Bool<false>());
	}

	~Transition()
	{
		Transition<Host, Destination, Source, Destination>::enterActions(mHost, Bool<false>());
		Destination::init(mHost);
	}

	Host& mHost;
};

template <typename T>
struct Initial
{
	typedef typename T::Host Host;

	Initial(Host& h) : mHost(h) {}
	~Initial()
	{
		T::enter(mHost);
		T::init(mHost);
	}

	Host& mHost;
};

typedef CompositeState<int, 0> S0;
typedef CompositeState<int, 11, S0> SS1;
typedef CompositeState<int, 22, S0> SS2;
typedef LeafState<int, 1, SS1> S1;
typedef LeafState<int, 2, SS2> S2;

void S0::init(int& n)
{
	cout << "S0::init\n";
	Initial<SS1> i(n);
}
void SS1::init(int& n)
{
	cout << "SS1::init\n";
	Initial<S1> i(n);
}
void SS2::init(int& n)
{
	cout << "SS2::init\n";
	Initial<S2> i(n);
}

void SS1::enter(int&)
{
	cout << "SS1::enter\n";
}
void SS1::exit(int&)
{
	cout << "SS1::exit\n";
}
void SS2::enter(int&)
{
	cout << "SS2::enter\n";
}
void SS2::exit(int&)
{
	cout << "SS2::exit\n";
}
void S1::enter(int&)
{
	cout << "S1::enter\n";
}
void S1::exit(int&)
{
	cout << "S1::exit\n";
}
void S1::handle(int&)
{
	cout << "S1::handle\n";
}

void S2::enter(int&)
{
	cout << "S2::enter\n";
}
void S2::exit(int&)
{
	cout << "S::exit\n";
}
void S2::handle(int&)
{
	cout << "2::handle\n";
}

int main()
{
	int n;
	{
		S0::init(n);
		Transition<int, S1, S1, SS2> t(n);
	}
	return 0;
}