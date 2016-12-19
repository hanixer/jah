package frontend.clike.parsers;

import frontend.Scanner;
import frontend.clike.ClikeTokenType;
import intermediate.ICodeFactory;
import intermediate.ICodeNode;
import intermediate.icodeimpl.ICodeNodeTypeImpl;

public class CompoundParser extends StatementParser {

    public CompoundParser(Scanner scanner) {
	super(scanner);
    }

    public ICodeNode parseCompound() throws Exception {
	int line = currentToken().getLineNumber();
	consumeExpected(ClikeTokenType.L_BRACKET);
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
	
	consumeExpected(ClikeTokenType.R_BRACKET);
	
	return node;
    }
}
