package frontend.clike;

import frontend.EofToken;
import frontend.Parser;
import frontend.Scanner;
import frontend.Token;
import frontend.TokenType;
import frontend.clike.parsers.StatementParser;
import intermediate.ICodeFactory;
import intermediate.ICodeNode;
import intermediate.SymTabEntry;
import message.Message;
import message.MessageType;

public class ClikeParser extends Parser {
    protected static ErrorHandler errorHandler;
    
    public ClikeParser(Scanner scanner) {
	super(scanner);
	iCode = ICodeFactory.createICode();
	errorHandler = new ErrorHandler();
    }

    @Override
    public void parse() throws Exception {
	long currentMs = System.currentTimeMillis();
	
	nextToken();
	StatementParser parser = new StatementParser(scanner);
	ICodeNode node = parser.parseStatement();
	iCode.setRootNode(node);
	
	long elapsed = System.currentTimeMillis() - currentMs;

	sendMessage(new Message(MessageType.PARSER_SUMMARY,
		new Object[] { 0, getErrorCount(), elapsed }));
    }

    @Override
    public int getErrorCount() {
	return errorHandler.getErrorCount();
    }

    public void consumeExpected(TokenType type) throws Exception {
	if (currentToken().getType() != type) {
	    errorHandler.flag(currentToken(), ErrorType.NONEXPECTED_TOKEN, this);
	}
	nextToken();
    }
}
