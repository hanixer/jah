

template <typename AH>
class StateMachine
{
public:
	StateMachine(AH& ah)
		: mAh(ah)
	{}

	static void addState(const std::string& name, std::function<void(AH&)> enter, std::function<void(AH&)> exit,
		std::function<void(AH&)> handler, const std::string& parentName, bool isInitial)
	{
		auto it = mStates.find(name);
		if (it != mStates.end()) throw std::exception("this name already used");

		State<AH> state(mAh);
		if (parentName.empty())
			state.pParent = 0;
		else
		{
			it = mStates.find(parentName);
			if (it == mStates.end()) throw std::exception("no such parent");

			state.pParent = it->second;
		}

		if (isInitial)
		{
			if (state.pParent)
			{
				if (state.pParent->pInitial)
					throw std::exception("parent already has initial state");
				else
					state.pParent->pInitial = state;
			}
			else
			{
				if (mpInitialState)
					throw std::exception("machine already has initial state");
				else
					mpInitialState = state;
			}
		}

		state.name = name;
		state.enter = enter;
		state.exit = exit;
		state.handler = handler;
		state.level = state.pParent == nullptr ? 1 : state.pParent->level + 1;
	}

	void changeState(const std::string& name)
	{
		auto it = mStates.find(name);
		if (it == mStates.end()) throw std::exception("no such state!!!");

		std::list<State<AH>*> exitStates;
		std::list<State<AH>*> enterStates;

		State<AH>* pState = &(it->second);
		int minLevel = mpCurrentState->level > pState->level ? pState->level : mpCurrentState->level;

		while (mpCurrentState->level != minLevel)
		{
			exitStates.push_back(mpCurrentState);
			mpCurrentState = mpCurrentState->pParent;
		}

		while (pState->level != minLevel)
		{
			enterStates.push_front(pState);
			pState = pState->pParent;
		}

		assert(mpCurrentState && pState && mpCurrentState->level == pState->level);

		while (mpCurrentState && pState && mpCurrentState->pParent != pState->pParent)
		{
			exitStates.push_back(mpCurrentState);
			enterStates.push_front(pState);
			mpCurrentState = mpCurrentState->pParent;
			pState = pState->pParent;
		}

		for (State<AH>* pS : exitStates)
		{
			pS->exit();
		}

		for (State<AH>* pS : enterStates)
		{
			pS->enter();
			mpCurrentState = pS;
		}
	}

private:
	std::map<std::string, State<AH> > mStates;
	State<AH>* mpInitialState;
	State<AH>* mpCurrentState;
	AH& mAh;
};