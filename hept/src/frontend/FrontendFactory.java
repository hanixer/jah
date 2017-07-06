package frontend;

import frontend.clike.ClikeParser;
import frontend.clike.ClikeScanner;

public class FrontendFactory {
    public static Parser createParser(String language, Source source) throws Exception {
	if (language.equalsIgnoreCase("Clike")) {
	    Scanner scanner = new ClikeScanner(source);
	    return new ClikeParser(scanner);
	} else {
	    throw new Exception("Parser Factory: don't know how" + " to make parser for languge " + language);
	}
    }
}
