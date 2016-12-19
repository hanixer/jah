package backend.interpreter.executors;

import backend.interpreter.Executor;
import backend.interpreter.RuntimeErrorType;
import frontend.clike.ClikeTokenType;
import intermediate.ICodeNode;
import intermediate.SymTabEntry;
import intermediate.icodeimpl.ICodeNodeKeyImpl;
import intermediate.icodeimpl.ICodeNodeTypeImpl;
import intermediate.symtabimpl.SymTabKeyImpl;

public class ExpressionExecutor extends Executor {

    Object execute(ICodeNode node) {
	Object value = null;
	
	switch ((ICodeNodeTypeImpl) node.getType()) {
	case VARIABLE: {
	    String rhsName = (String) node.getAttribute(ICodeNodeKeyImpl.ID);
	    SymTabEntry rhsSymEntry = symTab.lookup(rhsName);

	    if (rhsSymEntry == null) {
		errorHandler.flag(node, RuntimeErrorType.UNINITIALIZED_VALUE, this);
	    } else {
		value = rhsSymEntry.getAttribute(SymTabKeyImpl.DATA_VALUE);
	    }
	    break;
	}

	case INTEGER_CONSTANT:
	case REAL_CONSTANT: {
	    value = node.getAttribute(ICodeNodeKeyImpl.VALUE);
	    break;
	}

	case BINARY: {
	    ICodeNode lhs = node.getChildren().get(0);
	    ICodeNode rhs = node.getChildren().get(1);
	    Object lhsValue = execute(lhs);
	    Object rhsValue = execute(rhs);
	    
	    ClikeTokenType op = (ClikeTokenType) node.getAttribute(ICodeNodeKeyImpl.OP);
	    switch (op) {
	    case PLUS:
		value = (Integer) lhsValue + (Integer) rhsValue;
		break;
	    case MINUS:
		value = (Integer) lhsValue - (Integer) rhsValue;
		break;
	    case STAR:
		value = (Integer) lhsValue * (Integer) rhsValue;
		break;
	    case SLASH:
		value = (Integer) lhsValue / (Integer) rhsValue;
		break;
	    default:
		break;
	    }
	    break;
	}
	default:
	    break;

	}

	return value;
    }

}
