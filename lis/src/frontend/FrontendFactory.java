package frontend;

import java.io.BufferedReader;
import java.io.CharArrayReader;

import frontend.lis.LisParser;
import frontend.lis.LisScanner;

public class FrontendFactory {
    public static Parser createParser(String language, Source source) throws Exception {
	if (language.equalsIgnoreCase("Clike")) {
	    Scanner scanner = new LisScanner(source);
	    return new LisParser(scanner);
	} else {
	    throw new Exception("Parser Factory: don't know how" + " to make parser for languge " + language);
	}
    }
    
    public static Scanner createScanner(String string) throws Exception {
	Source source = new Source(new BufferedReader(
		new CharArrayReader(string.toCharArray())));
	return new LisScanner(source);
    }
}
