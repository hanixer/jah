package frontend.clike;

import frontend.EofToken;
import frontend.Parser;
import frontend.Scanner;
import frontend.Token;
import intermediate.SymTabEntry;
import message.Message;
import message.MessageType;

public class ClikeParser extends Parser {
    public ClikeParser(Scanner scanner) {
	super(scanner);
    }

    @Override
    public void parse() throws Exception {
	Token token;
	long currentMs = System.currentTimeMillis();

	while (!((token = scanner.nextToken()) instanceof EofToken)) {
	    
	    if (token.getType() == ClikeTokenType.IDENTIFIER) {
		SymTabEntry symTabEntry = symTabStack.lookup(token.getText());
		
		if (symTabEntry == null) {
		    symTabEntry = symTabStack.enterLocal(token.getText());
		}
		
		symTabEntry.appendLineNumber(token.getLineNumber());
	    }
	    
	    sendMessage(new Message(MessageType.TOKEN, 
		    new Object[] {
			    token.getLineNumber(),
			    token.getPosition(),
			    token.getType(),
			    token.getText(),
			    token.getValue()
		    }));
	}

	long elapsed = System.currentTimeMillis() - currentMs;

	sendMessage(new Message(MessageType.PARSER_SUMMARY,
		new Object[] { token.getLineNumber(), getErrorCount(), elapsed }));
    }

    @Override
    public int getErrorCount() {
	return 0;
    }

}
