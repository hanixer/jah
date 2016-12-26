package frontend.lis;

import java.util.EnumSet;

import frontend.TokenType;
import frontend.EofToken;
import frontend.Parser;
import frontend.Scanner;
import frontend.Token;
import frontend.lis.parsers.StatementParser;
import intermediate.ICodeFactory;
import intermediate.ICodeNode;
import message.Message;
import message.MessageType;

public class LisParser extends Parser {
    protected static ErrorHandler errorHandler;

    public LisParser(Scanner scanner) {
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

	sendMessage(new Message(MessageType.PARSER_SUMMARY, new Object[] { 0, getErrorCount(), elapsed }));
    }

    @Override
    public int getErrorCount() {
	return errorHandler.getErrorCount();
    }

    public void consumeExpected(TokenType type, ErrorType error) throws Exception {
	if (currentToken().getType() != type) {
	    errorHandler.flag(currentToken(), error, this);
	}
	nextToken();
    }

    public void semiExpected() throws Exception {
	if (currentToken().getType() != LisTokenType.LINE && currentToken().getType() != LisTokenType.SEMICOLON) {
	    errorHandler.flag(currentToken(), ErrorType.MISSING_STMT_SEPAR, this);
	}
	nextToken();
    }
    
    protected void flag(Token token, ErrorType error) {
	errorHandler.flag(token, error, this);
    }

    protected Token synchronize(EnumSet<?> syncSet) throws Exception {
	Token token = currentToken();

	if (!syncSet.contains(token.getType())) {
	    do {
		nextToken();
	    } while (!(currentToken() instanceof EofToken) && !syncSet.contains(token.getType()));
	}

	return token;
    }
}
