package backend.interpreter;

import backend.Backend;
import intermediate.ICodeNode;
import intermediate.icodeimpl.ICodeNodeKeyImpl;
import message.Message;
import message.MessageType;

public class RuntimeErrorHandler {
    private int errorCount;

    public RuntimeErrorHandler() {
    }

    public int getErrorCount() {
        return errorCount;
    }

    public void flag(ICodeNode node, RuntimeErrorType errorType, Backend backend) {
	while ((node != null) && (node.getAttribute(ICodeNodeKeyImpl.LINE) == null)) {
	    node = node.getParent();
	}
	
	if (node != null) {
	    backend.sendMessage(new Message(MessageType.RUNTIME_ERROR,
		    new Object[] {
			    node.getAttribute(ICodeNodeKeyImpl.LINE),
			    errorType.toString(),
		    }));
	}
    }
}
