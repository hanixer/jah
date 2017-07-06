package backend.interpreter.executors;

import backend.interpreter.Executor;
import intermediate.ICodeNode;
import intermediate.icodeimpl.ICodeNodeTypeImpl;

public class StatementExecutor extends Executor {
    public void execute(ICodeNode node) {
	switch ((ICodeNodeTypeImpl) node.getType()) {
	case COMPOUND:
	    new CompoundExecutor().execute(node);
	    break;
	case IF:
	    new IfExecutor().execute(node);
	    break;
	case LOOP:
	    new LoopExecutor().execute(node);
	    break;
	case EXPRESSION_STMT:
	    new ExpressionStatementExecutor().execute(node);
	    break;
	default:
	    break;
	}
	
    }
}
