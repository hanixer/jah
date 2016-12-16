package frontend.clike.tokens;

import static frontend.clike.ClikeTokenType.IDENTIFIER;

import frontend.Source;
import frontend.Token;

public class WordToken extends Token {

    public WordToken(Source source) throws Exception {
	super(source);
    }
    
    @Override
    public void extract() throws Exception {
        StringBuilder builder = new StringBuilder();
        char currentChar = currentChar();
        while (Character.isAlphabetic(currentChar) || Character.isDigit(currentChar) || currentChar == '_') {
            builder.append(currentChar);
            currentChar = nextChar();
        }
        
        type = IDENTIFIER;
        text = builder.toString();
    }

}
