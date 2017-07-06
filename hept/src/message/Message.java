package message;

public class Message {
    private Object body;
    protected MessageType type;

    public Message(MessageType type, Object body) {
	this.type = type;
	this.body = body;
    }

    public Object getBody() {
	return body;
    }

    public MessageType getType() {
	return type;
    }
}
