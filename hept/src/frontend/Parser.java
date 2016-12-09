package frontend;

import intermediate.ICode;
import intermediate.SymTab;
import message.Message;
import message.MessageHandler;
import message.MessageListener;
import message.MessageProducer;

/**
 * <h1>Parser</h1>
 * 
 */
public abstract class Parser implements MessageProducer {
    protected static MessageHandler messageHandler;

    static {
	messageHandler = new MessageHandler();
    }

    protected Scanner scanner;
    protected ICode iCode;
    private SymTab symTab;

    protected Parser(Scanner scanner) {
	this.scanner = scanner;
    }

    public abstract void parse() throws Exception;

    public abstract int getErrorCount();

    public ICode getICode() {
	return iCode;
    }

    public SymTab getSymTab() {
	return symTab;
    }

    public Token currentToken() {
	return scanner.currentToken();
    }

    public Token nextToken() throws Exception {
	return scanner.nextToken();
    }

    public void addMessageListener(MessageListener listener) {
	messageHandler.addMessageListener(listener);
    }

    public void removeMessageListener(MessageListener listener) {
	messageHandler.removeMessageListener(listener);
    }

    public void sendMessage(Message message) {
	messageHandler.sendMessage(message);
    }
}
