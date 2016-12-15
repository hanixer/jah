package frontend.clike.tokens;

import frontend.Source;
import frontend.Token;

import static frontend.clike.ClikeTokenType.*;
import static frontend.clike.ErrorType.*;

public class CharToken extends Token {

    public CharToken(Source source) throws Exception {
	super(source);
    }

    @Override
    protected void extract() throws Exception {
        char ch = nextChar();
        
        if (nextChar() == '\'') {
            nextChar();
            type = CHAR;
            value = ch;
            text = "" + ch;
        } else {
            type = ERROR;
            value = WRONG_CHAR;
        }
    }
}
