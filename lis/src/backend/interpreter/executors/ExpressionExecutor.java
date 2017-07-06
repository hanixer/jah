package backend.interpreter.executors;

import backend.interpreter.Executor;
import backend.interpreter.RuntimeErrorType;
import frontend.lis.LisTokenType;
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

	case BOOL_CONSTANT:
	case INTEGER_CONSTANT:
	case REAL_CONSTANT: {
	    value = node.getAttribute(ICodeNodeKeyImpl.VALUE);
	    break;
	}

	case BINARY: {
	    ICodeNode lhsNode = node.getChildren().get(0);
	    ICodeNode rhsNode = node.getChildren().get(1);
	    Object lhsVal = execute(lhsNode);
	    Object rhsVal = execute(rhsNode);

	    LisTokenType op = (LisTokenType) node.getAttribute(ICodeNodeKeyImpl.OP);

	    if (lhsVal instanceof Number && rhsVal instanceof Number) {
		// Numeric operations
		Double lhs = ((Number) lhsVal).doubleValue();
		Double rhs = ((Number) rhsVal).doubleValue();

		switch (op) {
		case PLUS:
		    value = lhs + rhs;
		    break;
		case MINUS:
		    value = lhs - rhs;
		    break;
		case STAR:
		    value = lhs * rhs;
		    break;
		case SLASH:
		    value = lhs / rhs;
		    break;
		case LESS_THEN:
		    value = lhs < rhs;
		    break;
		case LESS_EQ:
		    value = lhs <= rhs;
		    break;
		case GREATER_THEN:
		    value = lhs > rhs;
		    break;
		case GREATER_EQ:
		    value = lhs >= rhs;
		    break;
		case EQUAL:
		    value = lhs == rhs;
		    break;
		case NOT_EQUAL:
		    value = lhs != rhs;
		    break;
		default:

		    break;
		}
		break;
	    } else if (lhsVal instanceof Boolean && rhsVal instanceof Boolean) {
		// Boolean operations
		Boolean lhs = (Boolean) lhsVal;
		Boolean rhs = (Boolean) rhsVal;

		switch (op) {
		case AMPER_AMPER:
		    value = lhs && rhs;
		    break;
		case BAR_BAR:
		    value = lhs || rhs;
		case EQUAL:
		    value = lhs == rhs;
		    break;
		case NOT_EQUAL:
		    value = lhs != rhs;
		    break;
		default:
		    break;
		}
	    }
	}
	default:
	    break;

	}

	return value;
    }

}
