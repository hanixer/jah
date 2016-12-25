package frontend.lis.tokens;

import static frontend.lis.LisTokenType.*;
import static frontend.lis.ErrorType.*;

import frontend.Source;
import frontend.Token;

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
