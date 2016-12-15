package frontend;

public class Token {
    protected TokenType type;
    protected Source source;
    protected String text;
    protected Object value;
    protected int lineNum;
    protected int position;

    public Token(Source source) throws Exception {
	this.source = source;
	this.lineNum = source.getLineNum();
	this.position = source.getCurrentPos();

	extract();
    }

    protected void extract() throws Exception {
	text = Character.toString(currentChar());
	value = null;

	nextChar();
    }

    protected char nextChar() throws Exception {
	return source.nextChar();
    }

    public TokenType getType() {
        return type;
    }

    public int getLineNumber() {
	return lineNum;
    }

    public int getPosition() {
        return position;
    }

    public String getText() {
        return text;
    }

    public Object getValue() {
        return value;
    }

    protected char currentChar() throws Exception {
	return source.currentChar();
    }
}
