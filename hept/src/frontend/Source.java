package frontend;

import java.io.BufferedReader;
import java.io.IOException;

import message.Message;
import message.MessageHandler;
import message.MessageListener;
import message.MessageProducer;
import message.MessageType;

public class Source implements MessageProducer {
	public static final char EOL = '\n';
	public static final char EOF = 0;

	BufferedReader reader;
	String line;
	int lineNum;
	int currentPos;
	static MessageHandler messageHandler;

	static {
		messageHandler = new MessageHandler();
	}

	public Source(BufferedReader reader) {
		this.reader = reader;
		this.lineNum = 0;
		this.currentPos = -2;
	}

	public char nextChar() throws Exception {
		currentPos++;
		return currentChar();
	}

	public char currentChar() throws Exception {
		if (currentPos == -2) {
			readLine();
			return nextChar();
		} else if (line == null) {
			return EOF;
		} else if (currentPos == -1 || currentPos == line.length()) {
			return EOL;
		} else if (currentPos > line.length()) {
			readLine();
			return nextChar();
		} else {
			return line.charAt(currentPos);
		}
	}

	public char peekChar() throws Exception {
		if (line == null) {
			return EOF;
		}

		int nextPos = currentPos + 1;

		return nextPos < line.length() ? line.charAt(nextPos) : EOL;
	}

	public void close() throws Exception {
		if (reader != null) {
			try {
				reader.close();
			} catch (IOException e) {
				e.printStackTrace();
				throw e;
			}
		}
	}

	private void readLine() throws IOException {
		line = reader.readLine();

		currentPos = -1;

		if (line != null) {
			lineNum++;

			sendMessage(new Message(MessageType.SOURCE_LINE, new Object[] { lineNum, line }));
		}
	}

	@Override
	public void addMessageListener(MessageListener listener) {
		messageHandler.addMessageListener(listener);
	}

	@Override
	public void removeMessageListener(MessageListener listener) {
		messageHandler.removeMessageListener(listener);
	}

	@Override
	public void sendMessage(Message message) {
		messageHandler.sendMessage(message);
	}
}
