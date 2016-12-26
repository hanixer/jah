package frontend.lis.parsers;

import frontend.Scanner;
import frontend.lis.ErrorType;
import frontend.lis.LisTokenType;
import intermediate.ICodeFactory;
import intermediate.ICodeNode;
import intermediate.icodeimpl.ICodeNodeTypeImpl;

public class WhileStatementParser extends StatementParser {

    public WhileStatementParser(Scanner scanner) {
	super(scanner);
    }
    
    public ICodeNode parseStatement() throws Exception {
	ICodeNode node = ICodeFactory.createNode(ICodeNodeTypeImpl.LOOP);
	
	nextToken();
	
	ICodeNode cond = parseTest(STMT_END);
	
	if (cond == null) {
	    return ICodeFactory.createNode(ICodeNodeTypeImpl.NO_OP);
	}
	
	ICodeNode test = ICodeFactory.createNode(ICodeNodeTypeImpl.TEST);
	test.addChild(cond);
	
	node.addChild(test);
	
	ICodeNode body = new StatementParser(scanner).parseStatement();
	if (body == null) {
	    synchronize(STMT_END);
	    body = ICodeFactory.createNode(ICodeNodeTypeImpl.NO_OP);
	}
	node.addChild(body);
	
	return node;
    }

}
