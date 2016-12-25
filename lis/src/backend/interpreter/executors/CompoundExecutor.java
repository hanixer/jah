package backend.interpreter.executors;

import backend.interpreter.Executor;
import intermediate.ICodeNode;

public class CompoundExecutor extends Executor {

    public void execute(ICodeNode node) {
	for (ICodeNode child : node.getChildren()) {
	    new StatementExecutor().execute(child);
	}
    }
}
