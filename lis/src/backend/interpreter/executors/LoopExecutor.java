package backend.interpreter.executors;

import intermediate.ICodeNode;
import intermediate.icodeimpl.ICodeNodeTypeImpl;

public class LoopExecutor extends StatementExecutor {

    public LoopExecutor() {
	// TODO Auto-generated constructor stub
    }

    @Override
    public void execute(ICodeNode node) {
	ExpressionExecutor exprExec = new ExpressionExecutor();
	StatementExecutor stmtExec = new StatementExecutor();
	while (true) {
	    for (ICodeNode child : node.getChildren()) {
		if (child.getType() == ICodeNodeTypeImpl.TEST) {
		    boolean test = (boolean) exprExec.execute(child.getChildren().get(0));
		    if (!test) {
			return;
		    }
		} else {
		    stmtExec.execute(child);
		}
	    }
	}
    }
}
