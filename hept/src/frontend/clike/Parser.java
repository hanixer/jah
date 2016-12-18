package frontend.clike;

import java.io.BufferedReader;
import java.io.CharArrayReader;
import java.io.File;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.Map;

import frontend.Scanner;
import frontend.Source;
import frontend.Token;
import frontend.TokenType;
import intermediate.ICodeFactory;
import intermediate.ICodeNode;
import intermediate.icodeimpl.ICodeNodeKeyImpl;
import intermediate.icodeimpl.ICodeNodeTypeImpl;
import util.ParseTreePrinter;

/**
 * primary: identifier literal ( expression )
 *
 * postfix: primary postfix ++ postfix --
 * 
 * prefix: postfix ++ binary -- binary ! e ~ e + e - e
 * 
 * binary: e (+ | - | * | / | %) e
 * 
 * ternary: e ? e : e
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
	ICodeNode expr = parser.parseExpression(0);
	ICodeNode node = ICodeFactory.createCodeNode(ICodeNodeTypeImpl.PREFIX);
	node.setAttribute(ICodeNodeKeyImpl.OP, token.getType());
	node.addChild(expr);
	return node;
    }
}

class ParenParslet implements PrefixParslet {

    @Override
    public ICodeNode parse(Parser parser, Token token) throws Exception {
	ICodeNode expr = parser.parseExpression(0);
	parser.consume(ClikeTokenType.R_PAREN);
	return expr;
    }
    
}

interface InfixParslet {
    ICodeNode parse(Parser parser, ICodeNode left, Token token) throws Exception;

    int getPrecedence();
}

class InfixOperatorParslet implements InfixParslet {
    private int precedence;
    private boolean isRightAssoc;

    public InfixOperatorParslet(int precedence, boolean isRightAssoc) {
	this.precedence = precedence;
	this.isRightAssoc = isRightAssoc;
    }

    @Override
    public ICodeNode parse(Parser parser, ICodeNode left, Token token) throws Exception {
	ICodeNode right = parser.parseExpression(isRightAssoc ? precedence - 1 : precedence);
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

class PostfixOperatorParslet implements InfixParslet {
    private int precedence;
    
    public PostfixOperatorParslet(int precedence) {
	this.precedence = precedence;
    }

    @Override
    public ICodeNode parse(Parser parser, ICodeNode left, Token token) throws Exception {
	ICodeNode node = ICodeFactory.createCodeNode(ICodeNodeTypeImpl.POSTFIX);
	node.setAttribute(ICodeNodeKeyImpl.OP, token.getType());
	node.addChild(left);
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
    private Map<ClikeTokenType, PrefixParslet> prefixParslets = new HashMap<>();
    private Map<ClikeTokenType, InfixParslet> infixParslets = new HashMap<>();

    ICodeNode parseExpression(int precedence) throws Exception {
	Token token = currentToken();

	PrefixParslet prefix = prefixParslets.get(token.getType());

	if (prefix == null) {
	    return null;
	}
	
	nextToken();

	ICodeNode left = prefix.parse(this, token);

	while (true) {
	    token = currentToken();
	    InfixParslet infix = infixParslets.get(token.getType());

	    if (infix == null || precedence >= infix.getPrecedence()) {
		return left;
	    }

	    nextToken();

	    left = infix.parse(this, left, token);
	}
    }
    
    ICodeNode parseStatement() throws Exception {
	if (currentToken().getType() == ClikeTokenType.L_BRACKET) {
	    return parseCompound();
	}
	
	ICodeNode expr = parseExpression();
	ICodeNode node = null; 
	
	if (expr != null) {
	    node = ICodeFactory.createCodeNode(ICodeNodeTypeImpl.EXPRESSION_STMT);
	    node.addChild(expr);
	    consume(ClikeTokenType.SEMICOLON);
	} else if (currentToken().getType() == ClikeTokenType.SEMICOLON) {
	    nextToken();
	    
	    node = ICodeFactory.createCodeNode(ICodeNodeTypeImpl.NO_OP);
	}
	
	return node;
    }

    private ICodeNode parseCompound() throws Exception {
	consume(ClikeTokenType.L_BRACKET);
	ICodeNode node = ICodeFactory.createCodeNode(ICodeNodeTypeImpl.COMPOUND);
	
	while (true) {
	    ICodeNode stmt = parseStatement();
	    if (stmt != null) {
		node.addChild(stmt);
	    } else {
		break;
	    }
	}
	
	consume(ClikeTokenType.R_BRACKET);
	
	return node;
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
	prefixParslets.put(ClikeTokenType.L_PAREN, new ParenParslet());
	

	registerInfixRightAssoc(ClikeTokenType.ASSIGN, 1);
	registerInfix(ClikeTokenType.PLUS, 2);
	registerInfix(ClikeTokenType.MINUS, 2);
	registerInfix(ClikeTokenType.STAR, 3);
	registerInfix(ClikeTokenType.SLASH, 3);
	infixParslets.put(ClikeTokenType.PLUS_PLUS, new PostfixOperatorParslet(4));
	infixParslets.put(ClikeTokenType.MINUS_MINUS, new PostfixOperatorParslet(4));

	nextToken();
    }

    public void registerInfixRightAssoc(ClikeTokenType type, int precedence) {
	infixParslets.put(type, new InfixOperatorParslet(precedence, true));
    }

    public void registerInfix(ClikeTokenType type, int precedence) {
	infixParslets.put(type, new InfixOperatorParslet(precedence, false));
    }

    @Override
    public Token currentToken() {
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
    
    public void consume(TokenType type) throws Exception {
	if (currentToken().getType() != type) {
	    throw new Exception("Expected token not found " + type);
	}
	
	nextToken();
    }

    @Override
    public int getErrorCount() {
	// TODO Auto-generated method stub
	return 0;
    }

    public static void main(String[] args) throws Exception {
	String s = "{a + c a=b + c;a*-b;}";
	Source source = new Source(new BufferedReader(new CharArrayReader(s.toCharArray())));
	Scanner scanner = new ClikeScanner(source);
	Parser parser = new Parser(scanner);
	parser.parse();
	ParseTreePrinter printer = new ParseTreePrinter(System.out);
//	ParseTreePrinter printer = new ParseTreePrinter(new PrintStream(new File("out.xml")));
	printer.print(parser.parseStatement());
    }

    private ICodeNode parseExpression() throws Exception {
	return parseExpression(0);
    }
}
