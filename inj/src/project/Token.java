package project;

public class Token {
	public enum TokenKind {
		EOF,
		IDENTIFIER,
		NUMBER,
		OTHER
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
	
	public String toString() {
		return "[" + kind.toString() + " : " + text + "]";
	}
}
