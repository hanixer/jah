package frontend.lis.parsers;

import java.util.EnumSet;

import frontend.Scanner;
import frontend.TokenType;
import frontend.lis.ErrorType;
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
	skipSemi();
	
	switch ((LisTokenType) currTokenType()) {
	case L_BRACKET:
	    return new CompoundParser(scanner).parseStatement();
	case IF:
	    return new IfStatementParser(scanner).parseStatement();
	case WHILE:
	    return new WhileStatementParser(scanner).parseStatement();
	case DO:
	    return new DoWhileStatement(scanner).parseStatement();
	default:		
	}
	
	// Try to parse expression statement
	ExpressionParser exprParser = new ExpressionParser(scanner);
	ICodeNode expr = exprParser.parseExpression();
	ICodeNode node = null; 
	
	if (expr != null) {
	    node = ICodeFactory.createCodeNode(ICodeNodeTypeImpl.EXPRESSION_STMT, 
		    (Integer) expr.getAttribute(ICodeNodeKeyImpl.LINE));
	    node.addChild(expr);
	} 
	
	skipSemi();
	
	return node;
    }
    
    protected ICodeNode parseTest(EnumSet<?> end) throws Exception {
	consumeExpected(LisTokenType.L_PAREN, ErrorType.MISSING_LPAREN);
	
	ICodeNode cond = new ExpressionParser(scanner).parseExpression();
	
	if (cond == null) {
	    synchronize(end);
	}
	consumeExpected(LisTokenType.R_PAREN, ErrorType.MISSING_RPAREN);
	return cond;
    }

}
