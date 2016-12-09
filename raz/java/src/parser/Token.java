package parser;

public class Token {

    public TokenKind kind;
    public int startPos;
    public int endPos;
    public String text = "";

    Token(TokenKind kind) {
	this.kind = kind;
    }

    Token(TokenKind kind, String text) {
	this.kind = kind;
	this.text = text;
    }

    Token(TokenKind kind, int startPos, int endPos) {
	this.kind = kind;
	this.startPos = startPos;
	this.endPos = endPos;
    }

    Token(TokenKind kind, String text, int startPos, int endPos) {
	this.kind = kind;
	this.text = text;
	this.startPos = startPos;
	this.endPos = endPos;

	if (kind == TokenKind.IDENTIFIER) {
	    if (TokenKind.SIZEOF.value.compareTo(text) == 0) {
		this.kind = TokenKind.SIZEOF;
	    }
	}
    }

    public boolean isNot(TokenKind kind) {
	return this.kind != kind;
    }

    public boolean is(TokenKind kind) {
	return this.kind == kind;
    }

    public boolean isLiteral() {
	switch (kind) {
	case CHAR:
	case STRING:
	case NUMBER:
	    return true;
	case IDENTIFIER:
	    return text.compareTo("true") == 0 || text.compareTo("false") == 0 || text.compareTo("nullptr") == 0;
	default:
	    return false;
	}
    }

    public boolean isUnaryOperator() {
	switch (kind) {
	case STAR:
	case AMPERSAND:
	case PLUS:
	case MINUS:
	case EXCLAM:
	case TILDE:
	    return true;
	default:
	    return false;
	}
    }

    public boolean isAssignmentOperator() {
	switch (kind) {
	case ASSIGN:
	case STAR_ASSIGN:
	case SLASH_ASSIGN:
	case PERCENT_ASSIGN:
	case PLUS_ASSIGN:
	case MINUS_ASSIGN:
	case SHIFT_LEFT_ASSIGN:
	case SHIFT_RIGHT_ASSIGN:
	case AMPERSAND_ASSIGN:
	case HAT_ASSIGN:
	case BAR_ASSIGN:
	    return true;
	default:
	    return false;
	}
    }

    public int opPrecedence() {
	switch (kind) {
	case BAR_BAR:
	    return 1;
	case AMPER_AMPER:
	    return 2;
	case BAR:
	    return 3;
	case HAT:
	    return 4;
	case AMPERSAND:
	    return 5;
	case EQUAL:
	case NOT_EQUAL:
	    return 6;
	case LESS_THEN:
	case GREATER_THEN:
	case LESS_EQ:
	case GREATER_EQ:
	    return 7;
	case SHIFT_LEFT:
	case SHIFT_RIGHT:
	    return 8;
	case PLUS:
	case MINUS:
	    return 9;
	case STAR:
	case SLASH:
	case PERCENT:
	    return 10;
	default:
	    return 0xffffffff;
	}
    }

    public boolean isBinaryOperator() {
	switch (kind) {
	case BAR_BAR:
	case AMPER_AMPER:
	case BAR:
	case HAT:
	case AMPERSAND:
	case EQUAL:
	case NOT_EQUAL:
	case LESS_THEN:
	case GREATER_THEN:
	case LESS_EQ:
	case GREATER_EQ:
	case SHIFT_LEFT:
	case SHIFT_RIGHT:
	case PLUS:
	case MINUS:
	case STAR:
	case SLASH:
	case PERCENT:
	    return true;
	default:
	    return false;
	}
    }

    public String toString() {
	if (text.isEmpty())
	    return "[" + kind.toString() + " : (" + startPos + ", " + endPos + ")]";
	else
	    return "[" + kind.toString() + " : (" + startPos + ", " + endPos + ") : " + text + "]";
    }
}
