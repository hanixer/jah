package parser;

import java.nio.file.NotDirectoryException;
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
    int loopCounter = 0;

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

    SyntaxNode expression() {
	int sp = p;
	SyntaxNode asgnExpr = assignmentExpression();

	if (asgnExpr != null) {
	    do {
		if (tok.is(TokenKind.COMMA)) {
		    consume();
		    SyntaxNode asgnExpr2 = assignmentExpression();
		    if (asgnExpr2 == null) {
			restoreToken(sp);
			return null;
		    } else {
			asgnExpr = new SyntaxNode(NodeType.COMMA_EXPR, asgnExpr, asgnExpr2);
		    }
		} else {
		    return asgnExpr;
		}
	    } while (true);
	}
	
	restoreToken(p);
	return null;
    }
    
    SyntaxNode labeledStatement() {
	int sp = p;
	if (tok.is(TokenKind.IDENTIFIER)) {
	    SyntaxNode identNode = consumeAndMakeNode();
	    if (tok.is(TokenKind.COLON)) {
		consume();
		SyntaxNode stmt = statement();
		if (stmt != null) {
		    return new SyntaxNode(NodeType.LABELED_STMT, identNode, stmt);
		}
	    }
	}
	
	restoreToken(sp);
	return null;
    }
    
    SyntaxNode expressionStatement() {
	SyntaxNode expr = expression();
	if (expr != null && tok.is(TokenKind.SEMICOLON)) {
	    consume();
	    return new SyntaxNode(NodeType.EXPRESSION_STMT, expr);
	}
	
	return null;
    }
    
    SyntaxNode compoundStatement() {
	int sp = p;
	if (tok.is(TokenKind.L_BRACKET)) {
	    consume();
	    ArrayList<SyntaxNode> stmts = new ArrayList<>();
	    do {
		SyntaxNode stmt = statement();
		if (stmt == null) {
		    break;
		}
		stmts.add(stmt);
	    } while (true);
	    if (tok.is(TokenKind.R_BRACKET)) {
		consume();
		SyntaxNode[] ar = new SyntaxNode[stmts.size()];
		stmts.toArray(ar);
		return new SyntaxNode(NodeType.COMPOUND_STMT, ar);
	    }
	}
	restoreToken(sp);
	return null;
    }
    
    SyntaxNode selectionStatement() {
	int sp = p;
	if (tok.is(TokenKind.IF)) {
	    consume();
	    if (tok.is(TokenKind.L_PAREN)) {
		consume();
		SyntaxNode expr = expression();
		if (expr != null && tok.is(TokenKind.R_PAREN)) {
		    consume();
		    
		    SyntaxNode stmtThen = statement();
		    if (stmtThen != null) {
			int sp2 = p;
			if (tok.is(TokenKind.ELSE)) {
			    consume();
			    SyntaxNode stmtElse = statement();
			    if (stmtElse != null) {
				return new SyntaxNode(NodeType.IF_ELSE_STMT, expr, stmtThen, stmtElse);
			    }
			}
			
			restoreToken(sp2);
			return new SyntaxNode(NodeType.IF_STMT, expr, stmtThen);
		    }
		}
	    }
	}
	
	restoreToken(sp);
	return null;
    }
    
    SyntaxNode forStatement() {
	int sp = p;
	
	consume();
	
	if (tok.is(TokenKind.L_PAREN)) {
	    consume();
	    SyntaxNode expr1 = expression(); // TODO: declaration
	    if (tok.is(TokenKind.SEMICOLON)) {
		consume();
		SyntaxNode expr2 = expression();
		if (tok.is(TokenKind.SEMICOLON)) {
		    consume();
		    SyntaxNode expr3 = expression();
		    if (tok.is(TokenKind.R_PAREN)) {
			consume();
			loopCounter++;
			SyntaxNode stmt = statement();
			loopCounter--;
			if (stmt != null) {
			    return new SyntaxNode(NodeType.FOR_STMT, expr1, expr2, expr3, stmt);				    
			}
		    }
		}
	    }
	}
	
	restoreToken(sp);
	return null;
    }
    
    SyntaxNode whileStatement() {
	int sp = p;
	consume();
	if (tok.is(TokenKind.L_PAREN)) {
	    consume();
	    SyntaxNode expr = expression();
	    if (expr != null && tok.is(TokenKind.R_PAREN)) {
		consume();
		loopCounter++;
		SyntaxNode stmt = statement();
		loopCounter--;
		if (stmt != null) {
		    return new SyntaxNode(NodeType.WHILE_STMT, expr, stmt);
		}
	    }
	}
	
	restoreToken(sp);
	return null;
    }
    
    SyntaxNode doStatement() {
	int sp = p;
	consume();

	loopCounter++;
	SyntaxNode stmt = statement();
	loopCounter--;
	
	if (tok.is(TokenKind.WHILE)) {
	    consume();
	    if (tok.is(TokenKind.L_PAREN)) {
		consume();
		SyntaxNode expr = expression();
		if (expr != null && tok.is(TokenKind.R_PAREN)) {
		    consume();
		    if (tok.is(TokenKind.SEMICOLON)) {
			consume();
			return new SyntaxNode(NodeType.DO_STMT, stmt, expr);
		    }
		}
	    }
	}
	
	restoreToken(sp);
	return null;
    }
    
    SyntaxNode gotoStatement() {
	int sp = p;
	consume();
	if (tok.is(TokenKind.IDENTIFIER)) {
	    consume();
	    String ident = tok.text;
	    if (tok.is(TokenKind.SEMICOLON)) {
		consume();
		return new SyntaxNode(NodeType.GOTO_STMT, new SyntaxNode(ident));
	    }
	}
	
	restoreToken(sp);
	return null;
    }
    
    @SuppressWarnings("incomplete-switch")
    SyntaxNode statement() {
	switch (tok.kind) {
	case L_BRACKET:
	    return compoundStatement();
	case IF:
	    return selectionStatement();
	case FOR:
	    return forStatement();
	case WHILE:
	    return whileStatement();
	case DO:
	    return doStatement();
	case BREAK:
	    return breakStatement();
	case CONTINUE:
	    return continueStatement();
	case GOTO:
	    return gotoStatement();
	}
	
	SyntaxNode node = null;
	node = labeledStatement();
	if (node != null) {
	    return node;
	}
	
	node = expressionStatement();
	if (node != null) {
	    return node;		    
	}
	
	return node;
    }

    public SyntaxNode continueStatement() {
	if (loopCounter > 0) {
	consume();
	if (tok.is(TokenKind.SEMICOLON)) {
	    consume();
	    return new SyntaxNode(NodeType.CONTINUE_STMT);
	} else {
	    return null;
	}
	} else {
	return null;
	}
    }

    public SyntaxNode breakStatement() {
	if (loopCounter > 0) {
	consume();
	if (tok.is(TokenKind.SEMICOLON)) {
	    consume();
	    return new SyntaxNode(NodeType.BREAK_STMT);
	} else {
	    return null;
	}
	} else {
	return null;
	}
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
