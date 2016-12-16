package intermediate;

import intermediate.icodeimpl.ICodeImpl;
import intermediate.icodeimpl.ICodeNodeImpl;

public class ICodeFactory {
    
    public static ICode createICode() {
	return new ICodeImpl();
    }

    public static ICodeNode createCodeNode(ICodeNodeType type) {
	return new ICodeNodeImpl(type);
    }
}
