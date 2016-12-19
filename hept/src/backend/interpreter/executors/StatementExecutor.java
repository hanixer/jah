package backend.interpreter.executors;

import backend.interpreter.Executor;
import intermediate.ICodeNode;
import intermediate.icodeimpl.ICodeNodeTypeImpl;

public class StatementExecutor extends Executor {
    public void execute(ICodeNode codeNode) {
	if (codeNode.getType() == ICodeNodeTypeImpl.COMPOUND) {
	    new CompoundExecutor().execute(codeNode);
	} else if (codeNode.getType() == ICodeNodeTypeImpl.EXPRESSION_STMT) {
	    new ExpressionStatementExecutor().execute(codeNode);
	}
    }
}
