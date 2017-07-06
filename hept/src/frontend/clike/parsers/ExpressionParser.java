package frontend.clike.parsers;

import java.util.HashMap;
import java.util.Map;

import frontend.Scanner;
import frontend.Token;
import frontend.clike.ClikeTokenType;
import intermediate.ICodeFactory;
import intermediate.ICodeNode;
import intermediate.ICodeNodeType;
import intermediate.icodeimpl.ICodeNodeKeyImpl;
import intermediate.icodeimpl.ICodeNodeTypeImpl;

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
	parser.consumeExpected(ClikeTokenType.R_PAREN);
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
	ICodeNode node = ICodeFactory.createCodeNode(token.getType() == ClikeTokenType.ASSIGN ? ICodeNodeTypeImpl.ASSIGNMENT : ICodeNodeTypeImpl.BINARY, token.getLineNumber());
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

public class ExpressionParser extends frontend.clike.ClikeParser {
    private Map<ClikeTokenType, PrefixParslet> prefixParslets = new HashMap<>();
    private Map<ClikeTokenType, InfixParslet> infixParslets = new HashMap<>();

    public ExpressionParser(Scanner scanner) throws Exception {
	super(scanner);
	prefixParslets.put(ClikeTokenType.INTEGER, new ConstantParser(ICodeNodeTypeImpl.INTEGER_CONSTANT));
	prefixParslets.put(ClikeTokenType.REAL, new ConstantParser(ICodeNodeTypeImpl.REAL_CONSTANT));
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

    public void registerInfixRightAssoc(ClikeTokenType type, int precedence) {
	infixParslets.put(type, new InfixOperatorParslet(precedence, true));
    }

    public void registerInfix(ClikeTokenType type, int precedence) {
	infixParslets.put(type, new InfixOperatorParslet(precedence, false));
    }
}
