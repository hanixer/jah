package parser;

import java.util.ArrayList;

/**
 * {@code Parser p = new Parser();}
 * 
 * @author Chiteredy Onfe
 *
 */
public class Parser {
    Lexer lexer;
    Token[] tokens;
    Token tok;
    int p;
    Context context = new Context();

    public Parser(String source) {
	lexer = new Lexer(source);
	ArrayList<Token> ts = lexer.getTokens();
	tokens = new Token[ts.size()];
	tokens = ts.toArray(tokens);

	if (tokens.length > 0)
	    tok = tokens[0];
	else
	    tok = new Token(TokenKind.EOF);
    }

    SyntaxNode expression() {
	return null;
    }

    SyntaxNode primaryExpression() {
	int sp = p;

	if (tok.isLiteral() || tok.is(TokenKind.IDENTIFIER)) {
	    Token tok = consume();
	    SyntaxNode n = new SyntaxNode(tok);
	    n.type = NodeType.PRIMARY_EXPR;
	    return n;
	}

	p = sp;
	return null;
    }

    SyntaxNode postfixExpression() {
	SyntaxNode primExpr = primaryExpression();
	if (primExpr != null) {
	    SyntaxNode postfixExpr = postfixExpressionPart();
	    if (postfixExpr != null)
		return new SyntaxNode(NodeType.POSTFIX_EXPR, primExpr, postfixExpr);
	    else
		return new SyntaxNode(NodeType.POSTFIX_EXPR, primExpr);
	}

	return null;
    }

    SyntaxNode postfixExpressionPart() {
	SyntaxNode node = new SyntaxNode(NodeType.POSTFIX_EXPR);
	if (tok.is(TokenKind.PLUS_PLUS) || tok.is(TokenKind.MINUS_MINUS)) {
	    SyntaxNode opNode = consumeAndMakeNode();

	    SyntaxNode postfixExprPart = postfixExpressionPart();

	    if (postfixExprPart != null)
		node.addChildren(opNode, postfixExprPart);
	    else
		node.addChildren(opNode);

	    return node;
	}

	return null;
    }

    SyntaxNode unaryExpression() {
	int sp = p;
	SyntaxNode node = new SyntaxNode(NodeType.UNARY_EXPR);

	SyntaxNode pe = postfixExpression();
	if (pe != null) {
	    node.addChildren(pe);
	    return node;
	}

	p = sp;
	if (tok.is(TokenKind.PLUS_PLUS) || tok.is(TokenKind.MINUS_MINUS)) {
	    SyntaxNode incDecr = consumeAndMakeNode();
	    SyntaxNode castExpr = castExpression();
	    if (castExpr != null) {
		node.addChildren(incDecr, castExpr);
		return node;
	    }
	}

	restoreToken(sp);
	if (tok.isUnaryOperator()) {
	    SyntaxNode unarOper = consumeAndMakeNode();
	    SyntaxNode castExpr = castExpression();
	    if (castExpr != null) {
		node.addChildren(unarOper, castExpr);
		return node;
	    }
	}

	restoreToken(sp);
	if (isNextToken(TokenKind.SIZEOF)) {
	    SyntaxNode sizeofNode = consumeAndMakeNode();
	    SyntaxNode unaryExpr = unaryExpression();
	    if (unaryExpr != null) {
		node.addChildren(sizeofNode, unaryExpr);
		return node;
	    }

	    if (isNextToken(TokenKind.ELIPSIS)) {
		SyntaxNode elipseNode = consumeAndMakeNode();

		if (isNextToken(TokenKind.L_PAREN)) {
		    SyntaxNode lparenNode = consumeAndMakeNode();

		    if (isNextToken(TokenKind.IDENTIFIER)) {
			SyntaxNode identNode = consumeAndMakeNode();

			if (isNextToken(TokenKind.R_PAREN)) {
			    SyntaxNode rparenNode = consumeAndMakeNode();

			    node.addChildren(elipseNode, lparenNode, identNode, rparenNode);
			    return node;
			}
		    }
		}
	    }
	}

	restoreToken(sp);
	return null;
    }

    SyntaxNode castExpression() {
	return unaryExpression();
    }

    SyntaxNode pmExpression() {
	return castExpression();
    }

    SyntaxNode binaryExpression() {
	SyntaxNode pmExpr = pmExpression();
	if (pmExpr != null) {
	    return parseBinaryExpressionRhs(pmExpr, false);
	}

	return null;
    }

    SyntaxNode parseBinaryExpressionRhs(SyntaxNode lhs, boolean recurse) {
	while (tok.isBinaryOperator()) {
	    int prevPreced = tok.opPrecedence();
	    SyntaxNode op = consumeAndMakeNode();
	    SyntaxNode rhs = primaryExpression();
	    if (rhs != null) {
		if (tok.isBinaryOperator()) {
		    if (prevPreced < tok.opPrecedence()) {
			rhs = parseBinaryExpressionRhs(rhs, true);
			lhs = new SyntaxNode(NodeType.BINARY_EXPR, lhs, op, rhs);
		    } else {
			lhs = new SyntaxNode(NodeType.BINARY_EXPR, lhs, op, rhs);
			if (recurse)
			    return lhs;
		    }
		} else {
		    return new SyntaxNode(NodeType.BINARY_EXPR, lhs, op, rhs);
		}
	    } else
		return null;

	}

	return lhs;
    }

    SyntaxNode conditionalExpression() {
	int sp = p;
	SyntaxNode node = new SyntaxNode(NodeType.CONDITIONAL_EXPR);

	SyntaxNode binExpr = binaryExpression();
	if (binExpr != null) {
	    if (isNextToken(TokenKind.QUESTION)) {
		consume();

		SyntaxNode left = assignmentExpression(); // TODO: change to
							  // expression
		if (left == null)
		    return null;

		if (tok.isNot(TokenKind.COLON))
		    return null;

		consume();
		SyntaxNode right = assignmentExpression();
		if (right == null)
		    return null;

		node.addChildren(binExpr, left, right);
		return node;
	    } else {
		node.addChildren(binExpr);
		return node;
	    }
	}

	p = sp;
	return null;
    }

    SyntaxNode assignmentExpression() {
	int sp = p;

	SyntaxNode binLhs = binaryExpression();
	if (binLhs != null) {
	    if (tok.kind.name().contains("ASSIGN")) {
		SyntaxNode op = consumeAndMakeNode();
		SyntaxNode asgRhs = assignmentExpression();
		if (asgRhs != null) {
		    return new SyntaxNode(NodeType.ASSIGNMENT_EXPR, binLhs, op, asgRhs);
		}
	    }
	}

	restoreToken(sp);
	SyntaxNode condExpr = conditionalExpression();
	if (condExpr != null)
	    return condExpr;

	return null;
    }

    private Token consume() {
	Token prevTok = tok;
	if (tok.isNot(TokenKind.EOF)) {
	    p++;
	    tok = tokens[p];
	}
	return prevTok;
    }

    private boolean isNextToken(TokenKind kind) {
	if (p < tokens.length && tokens[p].kind == kind) {
	    return true;
	}
	return false;
    }

    private SyntaxNode consumeAndMakeNode() {
	Token opTok = consume();
	SyntaxNode opNode = new SyntaxNode(opTok);
	return opNode;
    }

    private void restoreToken(int savedPosition) {
	p = savedPosition;
	tok = tokens[p];
    }
}
