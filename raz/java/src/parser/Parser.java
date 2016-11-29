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
	int p;
	Context context = new Context();

	public Parser(String source) {
		lexer = new Lexer(source);
		ArrayList<Token> ts = lexer.getTokens();
		tokens = new Token[ts.size()];
		tokens = ts.toArray(tokens);
	}

	Node expression() {
		return null;
	}

	Node primaryExpression() {
		int sp = p;
		if (p < tokens.length) {
			if (tokens[p].isLiteral() || isNextToken(TokenKind.IDENTIFIER)) {
				Token tok = advance();
				return makeNode(tok);
			}
		}

		p = sp;
		return null;
	}

	Node postfixExpression() {
		int sp = p;
		Node pe = primaryExpression();
		if (pe != null) {
			Node pep = postfixExpressionPart();
			if (pep != null)
				return new Node("PostfixExpression", pe, pep);
			else
				return new Node("PostfixExpression", pe);
		}

		p = sp;
		return null;
	}

	Node postfixExpressionPart() {
		int sp = p;
		if (isNextToken(TokenKind.PLUS_PLUS) || isNextToken(TokenKind.MINUS_MINUS)) {
			Token tok = advance();
			return makeNode(tok);
		}

		p = sp;
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
		if (isNextToken(TokenKind.PLUS_PLUS) || isNextToken(TokenKind.MINUS_MINUS)) {
			Token tok = advance();
			Node incDecr = new Node(tok);
			Node ce = castExpression();
			if (ce != null) {
				node.addChildren(incDecr, ce);
				return node;
			}
		}

		p = sp;
		if (p < tokens.length && tokens[p].isUnaryOperator()) {
			Token tok = advance();
			Node unarOper = new Node(tok);
			Node ce = castExpression();
			if (ce != null) {
				node.addChildren(unarOper, ce);
				return node;
			}
		}

		p = sp;
		if (isNextToken(TokenKind.SIZEOF)) {
			Token sizeof = advance();
			Node sizeofNode = new Node(sizeof);
			int psizeof = p;
			Node ue = unaryExpression();
			if (ue != null) {
				node.addChildren(sizeofNode, ue);
				return node;
			}

			if (isNextToken(TokenKind.ELIPSIS)) {
				Token elipseTok = advance();
				Node elipseNode = new Node(elipseTok);

				if (isNextToken(TokenKind.L_PAREN)) {
					Token lparenTok = advance();
					Node lparenNode = new Node(lparenTok);

					if (isNextToken(TokenKind.IDENTIFIER)) {
						Token identTok = advance();
						Node identNode = new Node(identTok);

						if (isNextToken(TokenKind.R_PAREN)) {
							Token rparenTok = advance();
							Node rparenNode = new Node(rparenTok);

							node.addChildren(elipseNode, lparenNode, identNode, rparenNode);
							return node;
						}
					}
				}
			}
		}

		p = sp;
		return null;
	}

	Node castExpression() {
		return unaryExpression();
	}

	Node pmExpression() {
		return castExpression();
	}

	Node multiplicativeExpression() {
		int sp = p;
		Node node = new Node("MultiplicativeExpression");

		Node pme = pmExpression();
		if (pme != null) {
			if (isNextToken(TokenKind.STAR) || isNextToken(TokenKind.SLASH) || isNextToken(TokenKind.PERCENT)) {
				Token opTok = advance();
				Node opNode = new Node(opTok);

				Node me = multiplicativeExpression();
				if (me != null) {
					node.addChildren(pme, opNode, me);
					return node;
				}
			} else {
				node.addChildren(pme);
				return node;
			}
		}

		p = sp;
		return null;
	}

	Node additiveExpression() {
		int sp = p;
		Node node = new Node("AdditiveExpression");

		Node pme = multiplicativeExpression();
		if (pme != null) {
			if (isNextToken(TokenKind.PLUS) || isNextToken(TokenKind.MINUS)) {
				Token opTok = advance();
				Node opNode = new Node(opTok);

				Node ae = additiveExpression();
				if (ae != null) {
					node.addChildren(pme, opNode, ae);
					return node;
				}
			} else {
				node.addChildren(pme);
				return node;
			}
		}

		p = sp;
		return null;
	}

	Node shiftExpression() {
		int sp = p;
		Node node = new Node("ShiftExpression");

		Node additExpr = additiveExpression();
		if (additExpr != null) {
			int sp2 = p;
			if (isNextToken(TokenKind.SHIFT_LEFT) || isNextToken(TokenKind.SHIFT_RIGHT)) {
				Token shiftTok = advance();
				Node shiftNode = new Node(shiftTok);

				Node shiftExprNode = shiftExpression();
				if (shiftExprNode != null) {
					node.addChildren(additExpr, shiftNode, shiftExprNode);
					return node;
				}
			}
			p = sp2;
			node.addChildren(additExpr);
			return node;
		}

		p = sp;
		return null;
	}

	Node relationalExpression() {
		int sp = p;
		Node node = new Node("RelationalExpression");

		Node shiftExprNode = shiftExpression();
		if (shiftExprNode != null) {
			if (isNextToken(TokenKind.LESS_THEN) || isNextToken(TokenKind.GREATER_THEN)
					|| isNextToken(TokenKind.LESS_EQ) || isNextToken(TokenKind.GREATER_EQ)) {
				Token relopTok = advance();
				Node relopNode = new Node(relopTok);

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
				Token eqTok = advance();
				Node eqNode = new Node(eqTok);

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
				Token andTok = advance();
				Node andNode = new Node(andTok);

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
				Token xorTok = advance();
				Node xorNode = new Node(xorTok);

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
				Token iorTok = advance();
				Node iorNode = new Node(iorTok);

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
				Token logandTok = advance();
				Node logandNode = new Node(logandTok);

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
				Token logorTok = advance();
				Node logorNode = new Node(logorTok);

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
				Token qTok = advance();
				Node qNode = new Node(qTok);

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
				Token asgnTok = advance();
				Node asgnNode = new Node(asgnTok);

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
