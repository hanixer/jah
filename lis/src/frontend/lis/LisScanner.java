package frontend.lis;

import frontend.Scanner;
import frontend.Source;
import frontend.Token;
import frontend.lis.tokens.CharToken;
import frontend.lis.tokens.EofToken;
import frontend.lis.tokens.NumberToken;
import frontend.lis.tokens.SpecialSymbolToken;
import frontend.lis.tokens.StringToken;
import frontend.lis.tokens.WordToken;

import static frontend.Source.EOF;

public class LisScanner extends Scanner {

    public LisScanner(Source source) {
	super(source);
    }

    @Override
    protected Token extractToken() throws Exception {
	Token token = null;
	char ch = currentChar();

	if (ch != Source.EOL && Character.isWhitespace(ch)) {
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
	    token = new SpecialSymbolToken(source);
	}
	
	return token;
    }

    private void skipWhitespaces() throws Exception {
	while (true) {
	    char currentChar = source.nextChar();
	    if (!(Character.isWhitespace(currentChar))) {
		break;
	    }
	}
    }

}
