package frontend.lis;

import java.util.EnumSet;

import frontend.TokenType;
import frontend.Parser;
import frontend.Scanner;
import frontend.Token;
import frontend.lis.parsers.DeclarationParser;
import frontend.lis.parsers.StatementParser;
import frontend.lis.tokens.EofToken;
import intermediate.ICodeFactory;
import intermediate.ICodeNode;
import intermediate.icodeimpl.ICodeNodeTypeImpl;
import intermediate.symtabimpl.Predefined;
import message.Message;
import message.MessageType;

public class LisParser extends Parser {
    protected static ErrorHandler errorHandler;

    public LisParser(Scanner scanner) {
	super(scanner);
	iCode = ICodeFactory.createICode();
	errorHandler = new ErrorHandler();
    }
    
    public LisParser(LisParser parser) {
	super(parser.scanner);
	this.symTabStack = parser.symTabStack; 
    }

    @Override
    public void parse() throws Exception {
	long currentMs = System.currentTimeMillis();
	
	Predefined.initialize(symTabStack);
	symTabStack.push();

	nextToken();
	
	ICodeNode progNode = ICodeFactory.createNode(ICodeNodeTypeImpl.PROGRAM);
	
	while (currentToken().getType() != LisTokenType.EOF) {
	    DeclarationParser declParser = new DeclarationParser(this);
	    if (declParser.isDeclarationToken(currentToken())) {
		declParser.parse(currentToken().getType());
	    } else {
		StatementParser parser = new StatementParser(scanner);
		progNode.addChild(parser.parseStatement());
	    }
	}
	
	iCode.setRootNode(progNode);

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
    
    public Token nextToken() throws Exception {
	Token token = super.nextToken();
        sendMessage(new Message(MessageType.TOKEN, new Object[]{
        	token.getLineNumber(),
        	token.getPosition(),
        	token.getType(),
        	token.getText(),
        	token.getValue(),
        }));
        return token;
    }
    
    protected void flag(Token token, ErrorType error) {
	errorHandler.flag(token, error, this);
    }

    protected Token synchronize(EnumSet<?> syncSet) throws Exception {
	Token token = currentToken();

	if (!syncSet.contains(token.getType())) {
	    do {
		nextToken();
	    } while ((currentToken().getType() != LisTokenType.EOF) && !syncSet.contains(token.getType()));
	}

	return token;
    }
    
    protected void skipSemi() throws Exception {
	while (currTokenType() == LisTokenType.LINE || currTokenType() == LisTokenType.SEMICOLON) {
	    nextToken();
	}
    }

    protected TokenType currTokenType() {
	return currentToken().getType();
    }
}
