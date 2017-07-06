package intermediate;

import java.util.ArrayList;

public interface ICodeNode {
    ICodeNode getParent();
    ICodeNodeType getType();
    void addChild(ICodeNode iCodeNode);
    ArrayList<ICodeNode> getChildren();
    void setAttribute(ICodeNodeKey key, Object value);
    Object getAttribute(ICodeNodeKey key); 
    ICodeNode copy();
}
