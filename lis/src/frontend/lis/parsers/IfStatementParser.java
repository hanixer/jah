package frontend.lis.parsers;

import java.util.EnumSet;

import frontend.Scanner;
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
	nextToken();
	consumeExpected(LisTokenType.L_PAREN);
	
	ICodeNode cond = new ExpressionParser(scanner).parseExpression();
	
	if (cond == null) {
	    synchronize(IF_STMT_END);
	    return ICodeFactory.createNode(ICodeNodeTypeImpl.NO_OP);
	}
	
	consumeExpected(LisTokenType.R_PAREN);
	
	ICodeNode then = new StatementParser(scanner).parseStatement();
	if (then == null) {
	    synchronize(IF_STMT_END);
	    then = ICodeFactory.createNode(ICodeNodeTypeImpl.NO_OP);
	}
	
	if (currentToken().getType() == LisTokenType.ELSE) {
	    nextToken();
	    ICodeNode elseBody = new StatementParser(scanner).parseStatement();
	    if (elseBody == null) {
		synchronize(STMT_END);
		elseBody = ICodeFactory.createNode(ICodeNodeTypeImpl.NO_OP);		
	    }
	    
	    ICodeNode node = ICodeFactory.createNode(ICodeNodeTypeImpl.IF_ELSE);
	    node.addChild(cond);
	    node.addChild(then);
	    node.addChild(elseBody);
	    return node;
	} else {
	    ICodeNode node = ICodeFactory.createNode(ICodeNodeTypeImpl.IF);
	    node.addChild(cond);
	    node.addChild(then);
	    
	    return node;
	}
    }
}
