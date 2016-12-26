package backend.interpreter.executors;

import intermediate.ICodeNode;
import intermediate.SymTabEntry;
import intermediate.icodeimpl.ICodeNodeKeyImpl;
import intermediate.icodeimpl.ICodeNodeTypeImpl;
import intermediate.symtabimpl.SymTabKeyImpl;
import message.Message;
import message.MessageType;

public class ExpressionStatementExecutor extends StatementExecutor {

    public void execute(ICodeNode node) {
	ICodeNode child = node.getChildren().get(0);
	if (child.getType() == ICodeNodeTypeImpl.ASSIGNMENT) {
	    ICodeNode lhs = child.getChildren().get(0);
	    String lhsName = (String) lhs.getAttribute(ICodeNodeKeyImpl.ID);
	    SymTabEntry symEntry = symTab.lookup(lhsName);
	    
	    if (symEntry == null) symEntry = symTab.enter(lhsName);

	    ICodeNode rhs = child.getChildren().get(1);
	    ExpressionExecutor exprExecutor = new ExpressionExecutor();
	    Object value = exprExecutor.execute(rhs);
	    
	    symEntry.setAttribute(SymTabKeyImpl.DATA_VALUE, value);
	    
	    sendMessage(new Message(MessageType.ASSIGN,
		    new Object[] {
			    lhsName,
			    value == null ? "" : value.toString()
		    }));
	}
    }
}
