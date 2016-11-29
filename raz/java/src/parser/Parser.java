package parser;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Dictionary;
import java.util.Hashtable;

public class Parser {
	static class Node {
		public ArrayList<Node> childs = new ArrayList<>();
		public String tag;
		public TokenKind tkind;
		public Token token;
		private Dictionary<String, Object> attributes = new Hashtable<>();

		public Node(Token t, Node... nodes) {
			tag = null;
			token = t;
			tkind = token.kind;
			for (Node n : nodes) {
				childs.add(n);
			}
		}

		public Node(String s, Node... nodes) {
			tag = s;
			tkind = null;
			for (Node n : nodes) {
				childs.add(n);
			}
		}

		public void addChildren(Node... children) {
			childs.addAll(Arrays.asList(children));
		}
	}

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

	Node expression() {
		return null;
	}

	Node primaryExpression() {
		int sp = p;
		
		if (tok.isLiteral() || tok.is(TokenKind.IDENTIFIER)) {
			Token tok = consume();
			return makeNode(tok);
		}

		p = sp;
		return null;
	}

	Node postfixExpression() {
		Node primExpr = primaryExpression();
		if (primExpr != null) {
			Node postfixExpr = postfixExpressionPart();
			if (postfixExpr != null)
				return new Node("PostfixExpression", primExpr, postfixExpr);
			else
				return new Node("PostfixExpression", primExpr);
		}

		return null;
	}

	Node postfixExpressionPart() {
		Node node = new Node("PostfixExpressionPart");
		if (tok.is(TokenKind.PLUS_PLUS) || tok.is(TokenKind.MINUS_MINUS)) {
			Node opNode = consumeAndMakeNode();
			
			Node postfixExprPart = postfixExpressionPart();
			
			if (postfixExprPart != null) 
				node.addChildren(opNode, postfixExprPart);
			else 
				node.addChildren(opNode);
			
			return node;
		}

		return null;
	}

	Node unaryExpression() {
		int sp = p;
		Node node = new Node("UnaryExpression");

		Node pe = postfixExpression();
		if (pe != null) {
			node.addChildren(pe);
			return node;
		}

		p = sp;
		if (tok.is(TokenKind.PLUS_PLUS) || tok.is(TokenKind.MINUS_MINUS)) {
			Node incDecr = consumeAndMakeNode();
			Node castExpr = castExpression();
			if (castExpr != null) {
				node.addChildren(incDecr, castExpr);
				return node;
			}
		}

		restoreToken(sp);
		if (tok.isUnaryOperator()) {
			Node unarOper = consumeAndMakeNode();
			Node castExpr = castExpression();
			if (castExpr != null) {
				node.addChildren(unarOper, castExpr);
				return node;
			}
		}

		restoreToken(sp);
		if (isNextToken(TokenKind.SIZEOF)) {
			Node sizeofNode = consumeAndMakeNode();
			Node unaryExpr = unaryExpression();
			if (unaryExpr != null) {
				node.addChildren(sizeofNode, unaryExpr);
				return node;
			}

			if (isNextToken(TokenKind.ELIPSIS)) {
				Node elipseNode = consumeAndMakeNode();

				if (isNextToken(TokenKind.L_PAREN)) {
					Node lparenNode = consumeAndMakeNode();

					if (isNextToken(TokenKind.IDENTIFIER)) {
						Node identNode = consumeAndMakeNode();

						if (isNextToken(TokenKind.R_PAREN)) {
							Node rparenNode = consumeAndMakeNode();

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

	Node castExpression() {
		return unaryExpression();
	}

	Node pmExpression() {
		return castExpression();
	}

	Node multiplicativeExpression() {
		Node pmExpr = pmExpression();

		if (pmExpr != null) {
			Node node = new Node("MultiplicativeExpression");
			node.addChildren(pmExpr);
			
			int sp = p;
			if (isNextToken(TokenKind.STAR) || isNextToken(TokenKind.SLASH) || isNextToken(TokenKind.PERCENT)) {
				Node opNode = consumeAndMakeNode();

				Node multExpr = multiplicativeExpression();
				if (multExpr != null) {
					node.addChildren(opNode, multExpr);
					return node;
				}
			}
			
			restoreToken(sp);
			return node;
		}

		return null;
	}

	Node additiveExpression() {
		Node multExpr = multiplicativeExpression();

		if (multExpr != null) {
			Node node = new Node("AdditiveExpression");
			node.addChildren(multExpr);
			int sp = p;
			if (isNextToken(TokenKind.PLUS) || isNextToken(TokenKind.MINUS)) {
				Node opNode = consumeAndMakeNode();

				Node addExpr = additiveExpression();
				if (addExpr != null) {
					node.addChildren(opNode, addExpr);
					return node;
				}
			}
			
			restoreToken(sp);
			return node;
		}

		return null;
	}

	Node shiftExpression() {
		Node additExpr = additiveExpression();

		if (additExpr != null) {
			Node node = new Node("ShiftExpression");
			node.addChildren(additExpr);
			int sp = p;
			if (isNextToken(TokenKind.SHIFT_LEFT) || isNextToken(TokenKind.SHIFT_RIGHT)) {
				Node opNode = consumeAndMakeNode();

				Node shiftExprNode = shiftExpression();
				if (shiftExprNode != null) {
					node.addChildren(opNode, shiftExprNode);
					return node;
				}
			}
			
			restoreToken(sp);
			return node;
		}

		return null;
	}

	Node relationalExpression() {
		int sp = p;
		Node node = new Node("RelationalExpression");

		Node shiftExprNode = shiftExpression();
		if (shiftExprNode != null) {
			if (isNextToken(TokenKind.LESS_THEN) || isNextToken(TokenKind.GREATER_THEN)
					|| isNextToken(TokenKind.LESS_EQ) || isNextToken(TokenKind.GREATER_EQ)) {
				Node relopNode = consumeAndMakeNode();

				Node relExprNode = relationalExpression();
				if (relExprNode != null) {
					node.addChildren(shiftExprNode, relopNode, relExprNode);
					return node;
				}
			}
		}

		p = sp;
		return null;
	}

	Node equalityExpression() {
		int sp = p;
		Node node = new Node("EqualityExpression");

		Node relExprNode = relationalExpression();
		if (relExprNode != null) {
			if (isNextToken(TokenKind.EQUAL) || isNextToken(TokenKind.NOT_EQUAL)) {
				Node eqNode = consumeAndMakeNode();

				Node eqExpr = equalityExpression();
				if (eqExpr != null) {
					node.addChildren(relExprNode, eqNode, eqExpr);
					return node;
				}
			}
		}

		p = sp;
		return null;
	}

	Node andExpression() {
		int sp = p;
		Node node = new Node("AndExpression");

		Node eqExpr = equalityExpression();
		if (eqExpr != null) {
			if (isNextToken(TokenKind.AMPERSAND)) {
				Node andNode = consumeAndMakeNode();

				Node andExpr = andExpression();
				if (andExpr != null) {
					node.addChildren(eqExpr, andNode, andExpr);
					return node;
				}
			}
		}

		p = sp;
		return null;
	}

	Node xorExpression() {
		int sp = p;
		Node node = new Node("XorExpression");

		Node andExpr = andExpression();
		if (andExpr != null) {
			if (isNextToken(TokenKind.AMPERSAND)) {
				Node xorNode = consumeAndMakeNode();

				Node xorExpr = xorExpression();
				if (xorExpr != null) {
					node.addChildren(andExpr, xorNode, xorExpr);
					return node;
				}
			}
		}

		p = sp;
		return null;
	}

	Node iorExpression() {
		int sp = p;
		Node node = new Node("IorExpression");

		Node xorExpr = xorExpression();
		if (xorExpr != null) {
			if (isNextToken(TokenKind.AMPERSAND)) {
				Node iorNode = consumeAndMakeNode();

				Node iorExpr = iorExpression();
				if (iorExpr != null) {
					node.addChildren(xorExpr, iorNode, iorExpr);
					return node;
				}
			}
		}

		p = sp;
		return null;
	}

	Node logandExpression() {
		int sp = p;
		Node node = new Node("LogicalAndExpression");

		Node iorExpr = iorExpression();
		if (iorExpr != null) {
			if (isNextToken(TokenKind.AMPERSAND)) {
				Node logandNode = consumeAndMakeNode();

				Node logandExpr = logandExpression();
				if (logandExpr != null) {
					node.addChildren(iorExpr, logandNode, logandExpr);
					return node;
				}
			}
		}

		p = sp;
		return null;
	}

	Node logorExpression() {
		int sp = p;
		Node node = new Node("LogicalOrExpression");

		Node logandExpr = logandExpression();
		if (logandExpr != null) {
			if (isNextToken(TokenKind.AMPERSAND)) {
				Node logorNode = consumeAndMakeNode();

				Node logorExpr = logorExpression();
				if (logorExpr != null) {
					node.addChildren(logandExpr, logorNode, logorExpr);
					return node;
				}
			}
		}

		p = sp;
		return null;
	}

	Node conditionalExpression() {
		int sp = p;
		Node node = new Node("ConditionalExpression");

		Node logorExpr = logorExpression();
		if (logorExpr != null) {
			if (isNextToken(TokenKind.QUESTION)) {
				Node qNode = consumeAndMakeNode();

			} else {
				node.addChildren(logorExpr);
				return node;
			}
		}

		p = sp;
		return null;
	}

	Node assignmentExpression() {
		int sp = p;
		Node node = new Node("AssignmentExpression");

		Node condExpr = conditionalExpression();
		if (condExpr != null) {
			node.addChildren(condExpr);
			return node;
		}

		Node logorExpr = logorExpression();
		if (logorExpr != null) {
			if (p < tokens.length && tokens[p].isAssignmentOperator()) {
				Node asgnNode = consumeAndMakeNode();

				Node asgnExpr = assignmentExpression();
				if (asgnExpr != null) {
					node.addChildren(condExpr, asgnNode, asgnExpr);
					return node;
				}
			}
		}

		p = sp;
		return null;
	}

	private Node makeNode(Token tok) {
		Node n = new Node(tok);
		return n;
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


	private Node consumeAndMakeNode() {
		Token opTok = consume();
		Node opNode = new Node(opTok);
		return opNode;
	}

	private void restoreToken(int savedPosition) {
		p = savedPosition;
		tok = tokens[p];
	}
}
