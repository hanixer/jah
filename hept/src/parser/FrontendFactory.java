package parser;

import frontend.smallcpp.SmallCppParser;
import frontend.smallcpp.SmallCppScanner;

public class FrontendFactory {
    public static Parser createParser(String language, Source source) throws Exception {
	if (language.equalsIgnoreCase("SmallCpp")) {
	    Scanner scanner = new SmallCppScanner(source);
	    return new SmallCppParser(scanner);
	} else {
	    throw new Exception("Parser Factory: don't know how" + 
		    " to make parser for languge " + language);
	}
    }
}
