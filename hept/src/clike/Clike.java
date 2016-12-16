package clike;

import java.io.BufferedReader;
import java.io.FileReader;

import backend.Backend;
import backend.BackendFactory;
import frontend.FrontendFactory;
import frontend.Parser;
import frontend.Source;
import frontend.TokenType;
import intermediate.ICode;
import intermediate.SymTab;
import intermediate.SymTabStack;
import message.Message;
import message.MessageListener;

public class Clike {
    private Parser parser;
    private Source source;
    private SymTabStack symTabStack;

    @SuppressWarnings({ "unused", "unused" })
    public Clike(String operation, String filePath, String flags) {
	try {
	    boolean intermediate = flags.indexOf('i') > -1;
	    boolean xref = flags.indexOf('x') > -1;

	    source = new Source(new BufferedReader(new FileReader(filePath)));
	    source.addMessageListener(new SourceMessageListener());

	    parser = FrontendFactory.createParser("Clike", source);
	    parser.addMessageListener(new ParserMessageListener());

	    parser.parse();
	    source.close();

	    symTabStack = parser.getSymTabStack();
	    
	    CrossReferencer referencer = new  CrossReferencer();
	    referencer.print(symTabStack);
	    
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    public static void main(String[] args) {
	try {
	    String operation = args[0];
	    
	    int i = 0;
	    String flags = "";

	    while (++i < args.length && args[i].charAt(0) == '-') {
		flags += args[i].substring(1);
	    }

	    if (i < args.length) {
		String path = args[i];
		new Clike(operation, path, flags);
	    } else {
		throw new Exception("No files provided.");
	    }
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    private class SourceMessageListener implements MessageListener {

	private static final String SOURCE_LINE_FORMAT = "%03d %s";

	@Override
	public void messageReceived(Message message) {
	    Object[] body = (Object[]) message.getBody();

	    switch (message.getType()) {
	    case SOURCE_LINE:
		int lineNumber = (Integer) body[0];
		String lineText = (String) body[1];

		System.out.println(String.format(SOURCE_LINE_FORMAT, lineNumber, lineText));

		break;
	    default:
		break;
	    }

	}
    }

    private class ParserMessageListener implements MessageListener {

	private static final String PARSER_SUMMARY_FORMAT = "\n%,20d source lines." + "\n%,20d syntax errors."
		+ "\n%,20d ms seconds total.";
	
	private static final String TOKEN_FORMAT = ">>> %-20s line %d, position %d, text %s, value %s";

	@Override
	public void messageReceived(Message message) {
	    Object[] body = (Object[]) message.getBody();

	    switch (message.getType()) {
	    case PARSER_SUMMARY:
		int statementCount = (Integer) body[0];
		int errorsCount = (Integer) body[1];
		long elapsed = (Long) body[2];

		System.out.println(String.format(PARSER_SUMMARY_FORMAT, statementCount, errorsCount, elapsed));

		break;
	    case TOKEN:
		int lineNum = (Integer) body[0];
		int position = (Integer) body[1];
		TokenType type = (TokenType) body[2];
		String text = (String) body[3];
		Object value = (Object) body[4];

		if (type != null) {
		    System.out.println(String.format(TOKEN_FORMAT, type, lineNum, position, text, value));
		}
		
		break;
	    default:
		break;
	    }

	}
    }
}
