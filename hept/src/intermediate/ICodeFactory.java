package intermediate;

import intermediate.icodeimpl.ICodeImpl;
import intermediate.icodeimpl.ICodeNodeImpl;
import intermediate.icodeimpl.ICodeNodeKeyImpl;

public class ICodeFactory {
    
    public static ICode createICode() {
	return new ICodeImpl();
    }

    public static ICodeNode createCodeNode(ICodeNodeType type) {
	return new ICodeNodeImpl(type);
    }

    public static ICodeNode createCodeNode(ICodeNodeType type, int lineNumber) {
	ICodeNode node = createCodeNode(type);
	node.setAttribute(ICodeNodeKeyImpl.LINE, lineNumber);
	return node;
    }
}
