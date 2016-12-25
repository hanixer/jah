package intermediate.icodeimpl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import intermediate.ICodeFactory;
import intermediate.ICodeNode;
import intermediate.ICodeNodeKey;
import intermediate.ICodeNodeType;

public class ICodeNodeImpl extends HashMap<ICodeNodeKey, Object> implements ICodeNode {
    private static final long serialVersionUID = 7846042280206328019L;
    
    private ICodeNode parent;
    private ICodeNodeType type;
    private ArrayList<ICodeNode> children = new ArrayList<>();
    
    public ICodeNodeImpl(ICodeNodeType type) {
	this.type = type;
    }

    @Override
    public ICodeNode getParent() {
	return parent;
    }

    @Override
    public ICodeNodeType getType() {
	return type;
    }

    @Override
    public void addChild(ICodeNode iCodeNode) {
	children.add(iCodeNode);
    }

    @Override
    public ArrayList<ICodeNode> getChildren() {
	return children;
    }

    @Override
    public void setAttribute(ICodeNodeKey key, Object value) {
	put(key, value);
    }

    @Override
    public Object getAttribute(ICodeNodeKey key) {
	return get(key);
    }

    @Override
    public ICodeNode copy() {
	ICodeNodeImpl node = (ICodeNodeImpl) ICodeFactory.createNode(type);
	
	for (Entry<ICodeNodeKey, Object> entry : entrySet()) {
	    node.put(entry.getKey(), entry.getValue());
	}
	
	return node;
    }
    
    @Override
    public String toString() {
        return type.toString();
    }
}
