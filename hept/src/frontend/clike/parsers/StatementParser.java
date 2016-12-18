package frontend.clike.parsers;

import frontend.Scanner;
import frontend.clike.ClikeParser;
import frontend.clike.ClikeTokenType;
import intermediate.ICodeFactory;
import intermediate.ICodeNode;
import intermediate.icodeimpl.ICodeNodeTypeImpl;

public class StatementParser extends ClikeParser {

    public StatementParser(Scanner scanner) {
	super(scanner);
    }
    
    public ICodeNode parseStatement() throws Exception {
	if (currentToken().getType() == ClikeTokenType.L_BRACKET) {
	    CompoundParser parser = new CompoundParser(scanner);
	    return parser.parseCompound();
	}
	
	ExpressionParser exprParser = new ExpressionParser(scanner);
	ICodeNode expr = exprParser.parseExpression();
	ICodeNode node = null; 
	
	if (expr != null) {
	    node = ICodeFactory.createCodeNode(ICodeNodeTypeImpl.EXPRESSION_STMT);
	    node.addChild(expr);
	    consumeExpected(ClikeTokenType.SEMICOLON);
	} else if (currentToken().getType() == ClikeTokenType.SEMICOLON) {
	    nextToken();
	    
	    node = ICodeFactory.createCodeNode(ICodeNodeTypeImpl.NO_OP);
	}
	
	return node;
    }

}
