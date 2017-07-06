package frontend.lis.tokens;

import frontend.Source;
import frontend.Token;

import static frontend.Source.EOF;
import static frontend.lis.LisTokenType.*;
import static frontend.lis.ErrorType.*;

public class StringToken extends Token {

    public StringToken(Source source) throws Exception {
	super(source);
    }
    
    @Override
    protected void extract() throws Exception {
        char ch = nextChar();
        StringBuilder builder = new StringBuilder();
        
        while (ch != '"' && ch != EOF) {
            builder.append(ch);
            ch = nextChar();
        }
        
        if (ch == '"') {
            nextChar();
            type = STRING;
            value = builder.toString();
            text = builder.toString();
        } else {
            type = ERROR;
            value = INVALID_STRING_LITERAL;
        }
    }

}
