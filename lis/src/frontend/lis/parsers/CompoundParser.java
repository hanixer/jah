package frontend.lis.parsers;

import frontend.Scanner;
import frontend.lis.ErrorType;
import frontend.lis.LisTokenType;
import intermediate.ICodeFactory;
import intermediate.ICodeNode;
import intermediate.icodeimpl.ICodeNodeTypeImpl;

public class CompoundParser extends StatementParser {

    public CompoundParser(Scanner scanner) {
	super(scanner);
    }

    public ICodeNode parseStatement() throws Exception {
	int line = currentToken().getLineNumber();
	consumeExpected(LisTokenType.L_BRACKET, ErrorType.MISSING_LBRACKET);
	ICodeNode node = ICodeFactory.createCodeNode(ICodeNodeTypeImpl.COMPOUND, line);
	
	StatementParser parser = new StatementParser(scanner);
	while (true) {
	    ICodeNode stmt = parser.parseStatement();
	    if (stmt != null) {
		node.addChild(stmt);
	    } else {
		break;
	    }
	}
	
	consumeExpected(LisTokenType.R_BRACKET, ErrorType.MISSING_RBRACKET);
	
	return node;
    }
}
