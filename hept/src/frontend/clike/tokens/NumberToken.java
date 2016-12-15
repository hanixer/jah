package frontend.clike.tokens;

import frontend.Source;
import frontend.Token;

import static frontend.clike.ClikeTokenType.*;
import static frontend.clike.ErrorType.*;

public class NumberToken extends Token {

    public NumberToken(Source source) throws Exception {
	super(source);
    }

    @Override
    protected void extract() throws Exception {
	StringBuilder builder = new StringBuilder();
	char ch = currentChar();
	while (Character.isDigit(ch)) {
	    builder.append(ch);
	    ch = nextChar();
	}
	
	if (ch == '.') {
	    
	} else {
	    try {
		value = Integer.parseInt(builder.toString());
		type = INTEGER;
		text = value.toString();
	    } catch (NumberFormatException e) {
		type = ERROR;
		value = INVALID_INTEGER;
		return;
	    }   
	}
    }
}
