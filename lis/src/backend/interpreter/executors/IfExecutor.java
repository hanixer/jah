package backend.interpreter.executors;

import backend.interpreter.RuntimeErrorType;
import intermediate.ICodeNode;

public class IfExecutor extends StatementExecutor {

    public IfExecutor() {
    }

    public void execute(ICodeNode node) {
	Object testVal = new ExpressionExecutor().execute(node.getChildren().get(0));
	if (testVal == null) {
	    flag(node, RuntimeErrorType.BAD_TEST_EXPRESSION);
	    return;
	}

	boolean test = (Boolean) testVal;
	StatementExecutor executor = new StatementExecutor();
	if (test) {
	    executor.execute(node.getChildren().get(1));
	} else if (node.getChildren().size() > 1) {
	    executor.execute(node.getChildren().get(2));
	}
    }
}
