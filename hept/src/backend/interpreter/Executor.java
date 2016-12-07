package backend.interpreter;

import backend.Backend;
import intermediate.ICode;
import intermediate.SymTab;
import message.Message;
import message.MessageType;

public class Executor extends Backend {

    @Override
    public void process(ICode iCode, SymTab symTab) throws Exception {
	long curMs = System.currentTimeMillis();
	long elapsed = curMs - curMs;
	int instructionCount = 0;
	int executionCount = 0;
	sendMessage(new Message(MessageType.COMPILER_SUMMARY, 
		new Object[] {
			executionCount,
			instructionCount,
			elapsed
		}));

    }

}
