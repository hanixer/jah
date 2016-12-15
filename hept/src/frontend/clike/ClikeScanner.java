package frontend.clike;

import frontend.EofToken;
import frontend.Scanner;
import frontend.Source;
import frontend.Token;
import frontend.clike.tokens.CharToken;
import frontend.clike.tokens.NumberToken;
import frontend.clike.tokens.StringToken;
import frontend.clike.tokens.WordToken;

import static frontend.Source.EOL;
import static frontend.Source.EOF;

public class ClikeScanner extends Scanner {

    public ClikeScanner(Source source) {
	super(source);
    }

    @Override
    protected Token extractToken() throws Exception {
	Token token = null;
	char ch = currentChar();

	if (Character.isWhitespace(ch)) {
	    skipWhitespaces();
	}

	ch = currentChar();

	if (ch == EOF) {
	    token = new EofToken(source);
	} else if (Character.isAlphabetic(ch) || ch == '_') {
	    token = new WordToken(source);
	} else if (ch == '\'') {
	    token = new CharToken(source);
	} else if (ch == '"') {
	    token = new StringToken(source);
	} else if (Character.isDigit(ch)) {
	    token = new NumberToken(source);
	} else {
	    token = new Token(source);
	}
	
	return token;
    }

    private void skipWhitespaces() throws Exception {
	while (true) {
	    char currentChar = source.nextChar();
	    if (!(Character.isWhitespace(currentChar) || (currentChar == EOL))) {
		break;
	    }
	}
    }

}
