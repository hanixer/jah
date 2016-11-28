import java.util.ArrayList;

import parser.Lexer;
import parser.Token;
import parser.TokenKind;

/**
 * 
 * Grammar:
 * program - fn-decl-list
 * 
 * fn-decl-list - fn-decl fn-decl-list
 *  - empty
 *  
 * fn-decl - type identifier param-list fn-decl-end
 * 
 * fn-decl-end - SEMICOLON
 *  - fn-body
 *  
 *  fn-body - L_BRACKET stmt-list R_BRACKET
 *  
 *  stmt-list - stmt stmt-list
 *   - empty
 *   
 *   
 *
 */

public class SimpleArithmeticParser {
    Lexer lexer;
    Token[] tokens;
    int p;

    class Node {
	public ArrayList<Node> childs = new ArrayList<>();
	public String tag;
	public TokenKind tkind;
	public Token token;

	public Node(Token t) {
	    tag = null;
	    token = t;
	    tkind = token.kind;
	}

	public Node(String s) {
	    tag = s;
	    tkind = null;
	}
    }

    public SimpleArithmeticParser(String source) {
	lexer = new Lexer(source);
	ArrayList<Token> ts = lexer.getTokens();
	tokens = new Token[ts.size()];
	tokens = ts.toArray(tokens);
    }

    Node parse() {
	return expr();
    }

    private Node expr() {
	return additive();
    }

    private Node additive() {
	Node m = multiplicative();
	if (m != null) {
	    if (isNextToken(TokenKind.PLUS) || isNextToken(TokenKind.MINUS)) {
		Token op = advance();
		Node a = additive();
		if (a != null) {
		    Node n = new Node("additive");
		    n.childs.add(m);
		    n.childs.add(new Node(op));
		    n.childs.add(a);
		    return n;
		}
	    } else {
		Node n = new Node("additive");
		n.childs.add(m);
		return n;
	    }
	}

	return null;
    }

    private Node multiplicative() {
	Node p = primary();
	if (p != null) {
	    if (isNextToken(TokenKind.STAR) || isNextToken(TokenKind.SLASH)) {
		Token op = advance();
		Node m = multiplicative();
		if (m != null) {
		    Node n = new Node("multiplicative");
		    n.childs.add(p);
		    n.childs.add(new Node(op));
		    n.childs.add(m);
		    return n;
		}
	    } else {
		Node n = new Node("multiplicative");
		n.childs.add(p);
		return n;
	    }
	}

	return null;
    }

    private Node primary() {
	if (isNextToken(TokenKind.L_PAREN)) {
	    Token lParen = advance();
	    Node e = expr();
	    if (e == null) {
		return null;
	    } else {
		if (isNextToken(TokenKind.R_PAREN)) {
		    Token rParen = advance();
		    Node n = new Node("primary");
		    n.childs.add(new Node(lParen));
		    n.childs.add(e);
		    n.childs.add(new Node(rParen));
		    return n;
		}
	    }
	} else if (isNextToken(TokenKind.NUMBER)) {
	    Token t = advance();
	    Node n = new Node("primary");
	    n.childs.add(new Node(t));
	    return n;
	}

	return null;
    }

    private Token advance() {
	Token t = tokens[p];
	p++;
	return t;
    }

    private boolean isNextToken(TokenKind kind) {
	if (p < tokens.length && tokens[p].kind == kind) {
	    return true;
	}
	return false;
    }
}
