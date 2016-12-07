package smallcpp;

import java.io.BufferedReader;
import java.io.FileReader;

import backend.Backend;
import backend.BackendFactory;
import intermediate.ICode;
import intermediate.SymTab;
import message.Message;
import message.MessageListener;
import parser.FrontendFactory;
import parser.Parser;
import parser.Source;

public class SmallCpp {
    private Parser parser;
    private Source source;
    private ICode iCode;
    private SymTab symTab;
    private Backend backend;
    
    public SmallCpp(String operation, String filePath, String flags) {
	try {
	    boolean intermediate = flags.indexOf('i') > -1;
	    boolean xref = flags.indexOf('x') > -1;
	    
	    source = new Source(new BufferedReader(new FileReader(filePath)));
	    source.addMessageListener(new SourceMessageListener());
	    
	    parser = FrontendFactory.createParser("SmallCpp", source);
	    parser.addMessageListener(new ParserMessageListener());
	    
	    backend = BackendFactory.createBackend(operation);
	    
	    parser.parse();
	    source.close();
	    
	    iCode = parser.getICode();
	    symTab = parser.getSymTab();
	    
	    backend.process(iCode, symTab);
	    
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
		new SmallCpp(operation, path, flags);
	    } else {
		throw new Exception("No files provided.");
	    }
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }
    
    private class SourceMessageListener implements MessageListener {

	private static final String SOURCE_LINE_FORMAT = 
		"%03d %s";

	@Override
	public void messageReceived(Message message) {
	    Object[] body = (Object[])message.getBody();
	    
	    switch (message.getType()) {
	    case SOURCE_LINE:
		int lineNumber = (Integer)body[0];
		String lineText = (String)body[1];
		
		System.out.println(String.format(SOURCE_LINE_FORMAT, 
			lineNumber, lineText));
		
		break;
	    default:
		break;
	    }
	    
	}	
    }
    
    private class ParserMessageListener implements MessageListener {

	private static final String PARSER_SUMMARY_FORMAT = 
		"\n%,20d source lines." +
		"\n%,20d syntax errors." + 
		"\n%,20l ms seconds total.";
		

	@Override
	public void messageReceived(Message message) {
	    Object[] body = (Object[])message.getBody();
	    
	    switch (message.getType()) {
	    case PARSER_SUMMARY:
		int statementCount = (Integer)body[0];
		int errorsCount = (Integer)body[1];
		long elapsed = (Long)body[2];
		
		System.out.println(String.format(PARSER_SUMMARY_FORMAT, 
			statementCount, errorsCount, elapsed));
		
		break;
	    default:
		break;
	    }
	    
	}	
    }
}
