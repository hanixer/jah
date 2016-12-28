package frontend.lis.tokens;

import frontend.Source;
import frontend.Token;
import frontend.lis.LisTokenType;

public class EofToken extends Token {

    public EofToken(Source source) throws Exception {
	super(source);
	type = LisTokenType.EOF;
    }

    @Override
    public void extract() throws Exception {

    }

}
