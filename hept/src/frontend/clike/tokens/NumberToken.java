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

	readDecimalDigits(builder);

	if (currentChar() == '.') {
	    nextChar();
	    builder.append('.');
	    if (isCurrentDigit()) {
		readDecimalDigits(builder);
	    } else {
		type = ERROR;
		value = INVALID_REAL;
	    }
	} else {
	    integerValue(builder);
	}

	if (type == null) {
	    if (currentChar() == 'E' || currentChar() == 'e') {
		builder.append(currentChar());
		nextChar();
		if (currentChar() == '+' || currentChar() == '-') {
		    builder.append(currentChar());
		}

		if (isCurrentDigit()) {
		    readDecimalDigits(builder);
		    doubleValue(builder);
		} else {
		    type = ERROR;
		    value = INVALID_REAL;
		}
	    } else {
		doubleValue(builder);
	    }
	}
    }

    public void integerValue(StringBuilder builder) {
	try {
	value = Integer.parseInt(builder.toString());
	type = INTEGER;
	text = value.toString();
	} catch (NumberFormatException e) {
	type = ERROR;
	value = INVALID_INTEGER;
	}
    }

    public void doubleValue(StringBuilder builder) {
	try {
	value = Double.parseDouble(builder.toString());
	type = REAL;
	text = value.toString();
	} catch (NumberFormatException e) {
	type = ERROR;
	value = INVALID_REAL;
	}
    }

    public boolean isCurrentDigit() throws Exception {
	return Character.isDigit(currentChar());
    }

    public void readDecimalDigits(StringBuilder builder) throws Exception {
	char ch = currentChar();
	while (Character.isDigit(ch)) {
	    builder.append(ch);
	    ch = nextChar();
	}
    }
}
