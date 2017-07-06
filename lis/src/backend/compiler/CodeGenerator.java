package backend.compiler;

import backend.Backend;
import intermediate.ICode;
import intermediate.SymTab;
import message.Message;
import message.MessageType;

public class CodeGenerator extends Backend {

    @Override
    public void process(ICode iCode, SymTab symTab) throws Exception {
	// TODO Auto-generated method stub
	long curMs = System.currentTimeMillis();
	long elapsed = curMs - curMs;
	int instructionCount = 0;
	sendMessage(new Message(MessageType.COMPILER_SUMMARY, new Object[] { instructionCount, elapsed }));
    }

}
