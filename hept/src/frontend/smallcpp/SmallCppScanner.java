package frontend.smallcpp;

import parser.EofToken;
import parser.Scanner;
import parser.Source;
import parser.Token;

public class SmallCppScanner extends Scanner {

    public SmallCppScanner(Source source) {
	super(source);
    }

    @Override
    protected Token extractToken() throws Exception {
	Token token;
	char currentChar = currentChar();
	
	if (currentChar == Source.EOF) {
	    token = new EofToken(source);
	} else {
	    token = new Token(source);
	}
	return token;
    }
    
    
}
