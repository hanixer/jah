package frontend;

import intermediate.ICode;
import intermediate.SymTabFactory;
import intermediate.SymTabStack;
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
    protected SymTabStack symTabStack;

    protected Parser(Scanner scanner) {
	this.scanner = scanner;
	this.symTabStack = SymTabFactory.createSymTabStack();
    }

    public abstract void parse() throws Exception;

    public abstract int getErrorCount();

    public ICode getICode() {
	return iCode;
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
    
    public SymTabStack getSymTabStack() {
        return symTabStack;
    }
}
