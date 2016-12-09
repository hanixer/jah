package frontend.smallcpp;

import frontend.EofToken;
import frontend.Scanner;
import frontend.Source;
import frontend.Token;

import static frontend.Source.EOL;
import static frontend.Source.EOF;

public class SmallCppScanner extends Scanner {

    public SmallCppScanner(Source source) {
	super(source);
    }

    @Override
    protected Token extractToken() throws Exception {
	Token token;
	char currentChar = currentChar();

	if (Character.isWhitespace(currentChar)) {
	    skipWhitespaces();
	}

	if (currentChar == EOF) {
	    token = new EofToken(source);
	} else {
	    token = new Token(source);
	}
	return token;
    }

    private void skipWhitespaces() throws Exception {
	while (true) {
	    char currentChar = source.nextChar();
	    if (Character.isWhitespace(currentChar) || (currentChar == EOL)) {
		break;
	    }
	}
    }

}
