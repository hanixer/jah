package frontend.lis.parsers;

import java.util.EnumSet;

import frontend.Scanner;
import frontend.lis.ErrorType;
import frontend.lis.LisTokenType;
import intermediate.ICodeFactory;
import intermediate.ICodeNode;
import intermediate.icodeimpl.ICodeNodeTypeImpl;

public class IfStatementParser extends StatementParser {
    protected static EnumSet<LisTokenType> IF_STMT_END;
    
    static {
	IF_STMT_END = EnumSet.of(LisTokenType.ELSE);
	IF_STMT_END.addAll(STMT_END);
    }

    public IfStatementParser(Scanner scanner) {
	super(scanner);
    }

    public ICodeNode parseStatement() throws Exception {
	ICodeNode node = ICodeFactory.createNode(ICodeNodeTypeImpl.IF);
	nextToken();
	
	ICodeNode cond = parseTest(IF_STMT_END);
	
	if (cond == null) {
	    return ICodeFactory.createNode(ICodeNodeTypeImpl.NO_OP);
	}
	
	node.addChild(cond);
	
	
	ICodeNode then = new StatementParser(scanner).parseStatement();
	if (then == null) {
	    synchronize(IF_STMT_END);
	    then = ICodeFactory.createNode(ICodeNodeTypeImpl.NO_OP);
	}
	node.addChild(then);
	
	if (currentToken().getType() == LisTokenType.ELSE) {
	    nextToken();
	    ICodeNode elseBody = new StatementParser(scanner).parseStatement();
	    if (elseBody == null) {
		synchronize(STMT_END);
		elseBody = ICodeFactory.createNode(ICodeNodeTypeImpl.NO_OP);		
	    }
	    
	    node.addChild(elseBody);
	}
	
	return node;
    }
}
