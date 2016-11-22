package project;

public class Token {
	public enum TokenKind {
		EOF, ERROR, COMMENT, IDENTIFIER, NUMBER, CHAR, STRING, OTHER, DOUBLE_POUND, POUND, DIVIDE
	}

	public TokenKind kind;
	public int startPos;
	public int endPos;
	public String text;

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

	public String toString() {
		return "[" + kind.toString() + " : " + text + "]";
	}
}
