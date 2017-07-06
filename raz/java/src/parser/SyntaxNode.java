package parser;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Dictionary;
import java.util.Hashtable;

class SyntaxNode {
    public ArrayList<SyntaxNode> childs = new ArrayList<>();
    public String tag;
    public NodeType type;
    public TokenKind tkind;
    public Token token;
    private Dictionary<String, Object> attributes = new Hashtable<>();

    public SyntaxNode(Token t, SyntaxNode... nodes) {
	tag = null;
	token = t;
	tkind = token.kind;
	for (SyntaxNode n : nodes) {
	    childs.add(n);
	}
    }

    public SyntaxNode(String s, SyntaxNode... nodes) {
	tag = s;
	tkind = null;
	for (SyntaxNode n : nodes) {
	    childs.add(n);
	}
    }

    public SyntaxNode(NodeType type, SyntaxNode... nodes) {
	this.type = type;
	tkind = null;
	for (SyntaxNode n : nodes) {
	    childs.add(n);
	}
    }

    public void addChildren(SyntaxNode... children) {
	childs.addAll(Arrays.asList(children));
    }

    public String toString() {
	if (type != null)
	    return type.name().toLowerCase();
	else if (tkind != null)
	    return tkind.name();
	else if (tag != null)
	    return tag;
	else
	    return "!NONAME_NODE!";
    }
}