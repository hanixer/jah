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
    }

    public boolean isLiteral() {
	switch (kind) {
	case CHAR:
	case STRING:
	case NUMBER:
	    return true;
	case IDENTIFIER:
	    return text.compareTo("true") == 0 || text.compareTo("false") == 0;
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
