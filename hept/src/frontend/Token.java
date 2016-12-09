package frontend;

public class Token {
	protected TokenType type;
	protected Source source;
	protected String text;
	protected Object value;
	private int lineNum;
	private int position;

	public Token(Source source) throws Exception {
		this.source = source;

		extract();
	}

	public void extract() throws Exception {
		text = Character.toString(currentChar());
		value = null;

		nextChar();
	}

	private void nextChar() throws Exception {
		source.nextChar();
	}

	public int getLineNumber() {
		return lineNum;
	}

	public char currentChar() throws Exception {
		return source.currentChar();
	}
}
