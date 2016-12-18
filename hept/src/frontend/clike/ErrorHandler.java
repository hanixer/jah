package frontend.clike;

import frontend.Token;
import message.Message;
import message.MessageType;

public class ErrorHandler {
    private int errorCount;

    public int getErrorCount() {
        return errorCount;
    }
    
    public void flag(Token token, ErrorType type, ClikeParser parser) {
	parser.sendMessage(new Message(MessageType.SYNTAX_ERROR, 
		new Object[] {
			token.getLineNumber(),
			token.getPosition(),
			token.getText(),
			type.toString()
		}));
    }
    
}
