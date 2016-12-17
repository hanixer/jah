package frontend.clike;

import java.io.BufferedReader;
import java.io.CharArrayReader;
import java.util.HashMap;
import java.util.Map;

import frontend.FrontendFactory;
import frontend.Scanner;
import frontend.Source;
import frontend.Token;
import intermediate.ICodeFactory;
import intermediate.ICodeNode;
import intermediate.icodeimpl.ICodeNodeKeyImpl;
import intermediate.icodeimpl.ICodeNodeTypeImpl;
import util.ParseTreePrinter;

/**
 * primary:
 * 	identifier
 * 	literal
 *	( expression )
 *
 * postfix:
 * 	primary
 * 	postfix ++
 * 	postfix --
 * 
 * prefix:
 * 	postfix
 * 	++ binary
 * 	-- binary
 * 	! e
 * 	~ e
 * 	+ e
 * 	- e
 * 
 * binary:
 * 	e (+ | - | * | / | %) e
 * 
 * ternary:
 * 	e ? e : e
 */

interface PrefixParslet {
    ICodeNode parse(Parser parser, Token token) throws Exception;
}

class NameParser implements PrefixParslet {
    @Override
    public ICodeNode parse(Parser parser, Token token) {
	ICodeNode node = ICodeFactory.createCodeNode(ICodeNodeTypeImpl.VARIABLE);
	node.setAttribute(ICodeNodeKeyImpl.ID, token.getText());
	return node;
    }
}

class PrefixOperatorParslet implements PrefixParslet {
    @Override
    public ICodeNode parse(Parser parser, Token token) throws Exception {
	ICodeNode expr = parser.parseExpression();
	ICodeNode node = ICodeFactory.createCodeNode(ICodeNodeTypeImpl.PREFIX);
	node.setAttribute(ICodeNodeKeyImpl.OP, token.getType());
	node.addChild(expr);
	return node;
    }
}

interface InfixParslet {
    ICodeNode parse(Parser parser, ICodeNode left, Token token) throws Exception;
    int getPrecedence();
}

class InfixOperatorParslet implements InfixParslet {
    private int precedence;
    public InfixOperatorParslet(int precedence) {
	this.precedence = precedence;
    }
    @Override
    public ICodeNode parse(Parser parser, ICodeNode left, Token token) throws Exception {
	ICodeNode right = parser.parseExpression();
	ICodeNode node = ICodeFactory.createCodeNode(ICodeNodeTypeImpl.BINARY);
	node.setAttribute(ICodeNodeKeyImpl.OP, token.getType());
	node.addChild(left);
	node.addChild(right);
	return node;
    }
    @Override
    public int getPrecedence() {
	return precedence;
    }
}

public class Parser extends frontend.Parser {
    private Token ct;
    private Token lt;
    private Map<ClikeTokenType, PrefixParslet> prefixParslets = 
	    new HashMap<>();
    private Map<ClikeTokenType, InfixParslet> infixParslets = 
	    new HashMap<>();
    
    ICodeNode parseExpression() throws Exception {
	Token token = currentToken();
	nextToken();
	
	PrefixParslet prefix = prefixParslets.get(token.getType());
	
	if (prefix == null) {
	    throw new Exception("Wrong token, prefix operator expected");
	}
	
	ICodeNode left = prefix.parse(this, token);
	
	token = currentToken();	
	InfixParslet infix = infixParslets.get(token.getType());
	
	if (infix == null) return left;
	
	nextToken();
	
	return infix.parse(this, left, token);
    }

    public Parser(Scanner scanner) throws Exception {
	super(scanner);
	prefixParslets.put(ClikeTokenType.IDENTIFIER, new NameParser());
	prefixParslets.put(ClikeTokenType.PLUS_PLUS, new PrefixOperatorParslet());
	prefixParslets.put(ClikeTokenType.MINUS_MINUS, new PrefixOperatorParslet());
	prefixParslets.put(ClikeTokenType.PLUS, new PrefixOperatorParslet());
	prefixParslets.put(ClikeTokenType.MINUS, new PrefixOperatorParslet());
	prefixParslets.put(ClikeTokenType.EXCLAM, new PrefixOperatorParslet());
	prefixParslets.put(ClikeTokenType.TILDE, new PrefixOperatorParslet());
	infixParslets.put(ClikeTokenType.PLUS, new InfixOperatorParslet(1));
	infixParslets.put(ClikeTokenType.MINUS, new InfixOperatorParslet(1));
	
	nextToken();
    }
    
    @Override
    public Token currentToken()  {
	if (ct == null) {
	    ct = super.currentToken();
	}
	
        return ct;
    }
    
    @Override
    public Token nextToken() throws Exception {
	if (lt != null) {
	    ct = lt;
	    lt = null;
	} else {
	    ct = super.nextToken();
	}
        return ct;
    }
    
    Token lookahead() throws Exception {
	if (lt != null) {
	    return lt;
	}
	lt = super.nextToken();
	return lt;
    }

    @Override
    public void parse() throws Exception {
	
    }

    @Override
    public int getErrorCount() {
	// TODO Auto-generated method stub
	return 0;
    }

    public static void main(String[] args) throws Exception {
	String s = "a - b - -c";
	Source source = new Source(new BufferedReader(new CharArrayReader(s.toCharArray())));
	Scanner scanner = new ClikeScanner(source);
	Parser parser = new Parser(scanner);
	parser.parse();
	ParseTreePrinter printer = new ParseTreePrinter(System.out);
	printer.print(parser.parseExpression());
    }
}
