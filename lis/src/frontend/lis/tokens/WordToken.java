package frontend.lis.tokens;

import static frontend.lis.LisTokenType.IDENTIFIER;

import frontend.Source;
import frontend.Token;
import frontend.lis.LisTokenType;

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

        type = LisTokenType.getIdentifierOrKeyword(builder.toString());
        text = builder.toString();
    }

}
