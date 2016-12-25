package frontend.lis.parsers;

import java.util.EnumSet;

import frontend.Scanner;
import frontend.lis.LisParser;
import frontend.lis.LisTokenType;
import intermediate.ICodeFactory;
import intermediate.ICodeNode;
import intermediate.icodeimpl.ICodeNodeKeyImpl;
import intermediate.icodeimpl.ICodeNodeTypeImpl;

public class StatementParser extends LisParser {
    protected static EnumSet<LisTokenType> STMT_END;
    
    static {
	STMT_END = EnumSet.of(LisTokenType.R_BRACKET);
    }
    
    public StatementParser(Scanner scanner) {
	super(scanner);
    }
    
    public ICodeNode parseStatement() throws Exception {
	while (currentToken().getType() == LisTokenType.LINE) {
	    nextToken();
	}
	
	if (currentToken().getType() == LisTokenType.L_BRACKET) {
	    CompoundParser parser = new CompoundParser(scanner);
	    return parser.parseCompound();
	} else if (currentToken().getType() == LisTokenType.IF) {
	    return new IfStatementParser(scanner).parseStatement();
	}
	
	ExpressionParser exprParser = new ExpressionParser(scanner);
	ICodeNode expr = exprParser.parseExpression();
	ICodeNode node = null; 
	
	if (expr != null) {
	    node = ICodeFactory.createCodeNode(ICodeNodeTypeImpl.EXPRESSION_STMT, 
		    (Integer) expr.getAttribute(ICodeNodeKeyImpl.LINE));
	    node.addChild(expr);
	} 
	
	while (currentToken().getType() == LisTokenType.LINE) {
	    nextToken();
	}
	
	return node;
    }
}
