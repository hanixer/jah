package frontend.clike;

import frontend.EofToken;
import frontend.Parser;
import frontend.Scanner;
import frontend.Token;
import message.Message;
import message.MessageType;

public class ClikeParser extends Parser {

    public ClikeParser(Scanner scanner) {
	super(scanner);
	// TODO Auto-generated constructor stub
    }

    @Override
    public void parse() throws Exception {
	Token token;
	long currentMs = System.currentTimeMillis();

	while (!((token = scanner.nextToken()) instanceof EofToken)) {
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
	// TODO Auto-generated method stub
	return 0;
    }

}