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
		kind = TokenKind.SIZEOF;
	    }
	}
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

    public String toString() {
	if (text.isEmpty())
	    return "[" + kind.toString() + " : (" + startPos + ", " + endPos + ")]";
	else
	    return "[" + kind.toString() + " : (" + startPos + ", " + endPos + ") : " + text + "]";
    }
}
