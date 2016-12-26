package backend.interpreter;

import backend.Backend;
import backend.interpreter.executors.StatementExecutor;
import intermediate.ICode;
import intermediate.ICodeNode;
import intermediate.SymTab;
import message.Message;
import message.MessageType;

public class Executor extends Backend {
    protected static RuntimeErrorHandler errorHandler = new RuntimeErrorHandler();
    protected static SymTab symTab;

    @Override
    public void process(ICode iCode, SymTab symTab) throws Exception {
	Executor.symTab = symTab;
	
	long curMs = System.currentTimeMillis();

	new StatementExecutor().execute(iCode.getRootNode());
	
	long elapsed = curMs - curMs;
	int instructionCount = 0;
	int executionCount = 0;
	sendMessage(
		new Message(MessageType.INTERPRETER_SUMMARY, new Object[] { executionCount, instructionCount, elapsed }));

    }

    protected void flag(ICodeNode node, RuntimeErrorType errorType) {
	errorHandler.flag(node, errorType, this);
    }
}
