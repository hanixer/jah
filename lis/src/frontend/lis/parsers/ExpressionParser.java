package frontend.lis.parsers;

import java.util.HashMap;
import java.util.Map;

import frontend.FrontendFactory;
import frontend.Scanner;
import frontend.Token;
import frontend.lis.ErrorType;
import frontend.lis.LisTokenType;
import intermediate.ICodeFactory;
import intermediate.ICodeNode;
import intermediate.ICodeNodeType;
import intermediate.icodeimpl.ICodeNodeKeyImpl;
import intermediate.icodeimpl.ICodeNodeTypeImpl;
import util.ParseTreePrinter;

interface PrefixParslet {
    ICodeNode parse(ExpressionParser parser, Token token) throws Exception;
}

class ConstantParser implements PrefixParslet {
    private ICodeNodeType nodeType;
    
    ConstantParser(ICodeNodeType nodeType) {
	this.nodeType = nodeType;
    }
    
    @Override
    public ICodeNode parse(ExpressionParser parser, Token token) {
	ICodeNode node = ICodeFactory.createCodeNode(nodeType, token.getLineNumber());
	node.setAttribute(ICodeNodeKeyImpl.VALUE, token.getValue());
	return node;
    }
}

class NameParser implements PrefixParslet {
    @Override
    public ICodeNode parse(ExpressionParser parser, Token token) {
	ICodeNode node = ICodeFactory.createCodeNode(ICodeNodeTypeImpl.VARIABLE, token.getLineNumber());
	node.setAttribute(ICodeNodeKeyImpl.ID, token.getText());
	return node;
    }
}

class BooleanParser implements PrefixParslet {
    @Override
    public ICodeNode parse(ExpressionParser parser, Token token) {
	ICodeNode node = ICodeFactory.createCodeNode(ICodeNodeTypeImpl.BOOL_CONSTANT, token.getLineNumber());
	node.setAttribute(ICodeNodeKeyImpl.VALUE, token.getText().equals("true"));
	return node;
    }
}

class PrefixOperatorParslet implements PrefixParslet {
    @Override
    public ICodeNode parse(ExpressionParser parser, Token token) throws Exception {
	ICodeNode expr = parser.parseExpression(0);
	ICodeNode node = ICodeFactory.createCodeNode(ICodeNodeTypeImpl.PREFIX, token.getLineNumber());
	node.setAttribute(ICodeNodeKeyImpl.OP, token.getType());
	node.addChild(expr);
	return node;
    }
}

class ParenParslet implements PrefixParslet {

    @Override
    public ICodeNode parse(ExpressionParser parser, Token token) throws Exception {
	ICodeNode expr = parser.parseExpression(0);
	parser.consumeExpected(LisTokenType.R_PAREN, ErrorType.MISSING_RPAREN);
	return expr;
    }

}

interface InfixParslet {
    ICodeNode parse(ExpressionParser parser, ICodeNode left, Token token) throws Exception;

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
    public ICodeNode parse(ExpressionParser parser, ICodeNode left, Token token) throws Exception {
	ICodeNode right = parser.parseExpression(isRightAssoc ? precedence - 1 : precedence);
	ICodeNode node = ICodeFactory.createCodeNode(token.getType() == LisTokenType.ASSIGN ? ICodeNodeTypeImpl.ASSIGNMENT : ICodeNodeTypeImpl.BINARY, token.getLineNumber());
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
    public ICodeNode parse(ExpressionParser parser, ICodeNode left, Token token) throws Exception {
	ICodeNode node = ICodeFactory.createCodeNode(ICodeNodeTypeImpl.POSTFIX, token.getLineNumber());
	node.setAttribute(ICodeNodeKeyImpl.OP, token.getType());
	node.addChild(left);
	return node;
    }

    @Override
    public int getPrecedence() {
	return precedence;
    }
}

public class ExpressionParser extends frontend.lis.LisParser {
    private Map<LisTokenType, PrefixParslet> prefixParslets = new HashMap<>();
    private Map<LisTokenType, InfixParslet> infixParslets = new HashMap<>();

    public ExpressionParser(Scanner scanner) throws Exception {
	super(scanner);
	prefixParslets.put(LisTokenType.INTEGER, new ConstantParser(ICodeNodeTypeImpl.INTEGER_CONSTANT));
	prefixParslets.put(LisTokenType.REAL, new ConstantParser(ICodeNodeTypeImpl.REAL_CONSTANT));
	prefixParslets.put(LisTokenType.IDENTIFIER, new NameParser());
	prefixParslets.put(LisTokenType.TRUE, new BooleanParser());
	prefixParslets.put(LisTokenType.FALSE, new BooleanParser());
	prefixParslets.put(LisTokenType.PLUS_PLUS, new PrefixOperatorParslet());
	prefixParslets.put(LisTokenType.MINUS_MINUS, new PrefixOperatorParslet());
	prefixParslets.put(LisTokenType.PLUS, new PrefixOperatorParslet());
	prefixParslets.put(LisTokenType.MINUS, new PrefixOperatorParslet());
	prefixParslets.put(LisTokenType.EXCLAM, new PrefixOperatorParslet());
	prefixParslets.put(LisTokenType.TILDE, new PrefixOperatorParslet());
	prefixParslets.put(LisTokenType.L_PAREN, new ParenParslet());

	registerInfixRightAssoc(LisTokenType.ASSIGN, 1);
	registerInfix(LisTokenType.BAR_BAR, 2);
	registerInfix(LisTokenType.AMPER_AMPER, 3);
	registerInfix(LisTokenType.EQUAL, 4);
	registerInfix(LisTokenType.NOT_EQUAL, 4);
	registerInfix(LisTokenType.LESS_THEN, 5);
	registerInfix(LisTokenType.LESS_EQ, 5);
	registerInfix(LisTokenType.GREATER_THEN, 5);
	registerInfix(LisTokenType.GREATER_EQ, 5);
	registerInfix(LisTokenType.PLUS, 6);
	registerInfix(LisTokenType.MINUS, 6);
	registerInfix(LisTokenType.STAR, 7);
	registerInfix(LisTokenType.SLASH, 7);
	infixParslets.put(LisTokenType.PLUS_PLUS, new PostfixOperatorParslet(8));
	infixParslets.put(LisTokenType.MINUS_MINUS, new PostfixOperatorParslet(8));
	
	if (currentToken() == null) 
	    nextToken();
    }

    public ICodeNode parseExpression() throws Exception {
	return parseExpression(0);
    }

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

    public void registerInfixRightAssoc(LisTokenType type, int precedence) {
	infixParslets.put(type, new InfixOperatorParslet(precedence, true));
    }

    public void registerInfix(LisTokenType type, int precedence) {
	infixParslets.put(type, new InfixOperatorParslet(precedence, false));
    }
    
    public static void main(String[] args) throws Exception {
	Scanner s = FrontendFactory.createScanner("d = c <= b");
	ExpressionParser p = new ExpressionParser(s);
	ParseTreePrinter printer = new ParseTreePrinter(System.out);
	printer.print(p.parseExpression());
    }
}
