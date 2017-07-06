package intermediate.icodeimpl;

import intermediate.ICode;
import intermediate.ICodeNode;

public class ICodeImpl implements ICode {
    private ICodeNode rootNode;

    public ICodeImpl() {
    }

    @Override
    public ICodeNode getRootNode() {
	return rootNode;
    }

    @Override
    public void setRootNode(ICodeNode iCodeNode) {
	rootNode = iCodeNode;
    }

}
