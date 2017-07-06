package frontend.lis.parsers;

import frontend.Scanner;
import frontend.lis.ErrorType;
import frontend.lis.LisTokenType;
import intermediate.ICodeFactory;
import intermediate.ICodeNode;
import intermediate.icodeimpl.ICodeNodeTypeImpl;

public class DoWhileStatement extends StatementParser {

    public DoWhileStatement(Scanner scanner) {
	super(scanner);
    }
    
    public ICodeNode parseStatement() throws Exception {
	ICodeNode node = ICodeFactory.createNode(ICodeNodeTypeImpl.LOOP);
	
	nextToken();
	
	ICodeNode body = new StatementParser(scanner).parseStatement();
	if (body == null) {
	    flag(currentToken(), ErrorType.MISSING_STMT);
	    synchronize(STMT_END);
	    return ICodeFactory.createNode(ICodeNodeTypeImpl.NO_OP);
	}
	node.addChild(body);
	
	consumeExpected(LisTokenType.WHILE, ErrorType.MISSING_WHILE);
	
	ICodeNode cond = parseTest(STMT_END);
	if (cond == null) {
	    flag(currentToken(), ErrorType.MISSING_CONDITION);
	    return ICodeFactory.createNode(ICodeNodeTypeImpl.NO_OP);
	}
	
	ICodeNode test = ICodeFactory.createNode(ICodeNodeTypeImpl.TEST);
	test.addChild(cond);
	
	node.addChild(test);
	
	return node;
    }

}
