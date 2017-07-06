package backend;

import intermediate.ICode;
import intermediate.SymTab;
import message.Message;
import message.MessageHandler;
import message.MessageListener;
import message.MessageProducer;

public abstract class Backend implements MessageProducer {
    static protected MessageHandler messageHandler;

    static {
	messageHandler = new MessageHandler();
    }

    protected ICode iCode;
    protected SymTab symTab;

    public abstract void process(ICode iCode, SymTab symTab) throws Exception;

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
